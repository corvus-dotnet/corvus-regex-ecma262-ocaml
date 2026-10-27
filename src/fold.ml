(* Case-insensitive matching (fold.rs of the Rust crate).

   ECMA-262 defines it through a Canonicalize function: two characters match when they canonicalize to the same
   character. The function differs between the two grammars. With the u or v flag it is Unicode simple case folding.
   With no flag it is the single character uppercase mapping of a UTF-16 code unit, which never maps a character
   outside ASCII into ASCII.

   The parser applies the equivalence to every literal and class as it reads them, so a case-insensitive pattern is
   matched with plain classes. Only a backreference needs the function when matching.

   The folding and the uppercase mapping come from the tables of Ucd, never from the standard library, so a pattern
   matches the same texts whatever built the program. *)

(* The characters of the Basic Multilingual Plane whose uppercase form is more than one character, as inclusive pairs.
   Canonicalize leaves them alone. *)
let multi_upper =
  Charset.make
    [|
      0xDF; 0xDF; 0x149; 0x149; 0x1F0; 0x1F0; 0x390; 0x390; 0x3B0; 0x3B0; 0x587; 0x587; 0x1E96; 0x1E9A; 0x1F50; 0x1F50;
      0x1F52; 0x1F52; 0x1F54; 0x1F54; 0x1F56; 0x1F56; 0x1F80; 0x1FAF; 0x1FB2; 0x1FB4; 0x1FB6; 0x1FB7; 0x1FBC; 0x1FBC;
      0x1FC2; 0x1FC4; 0x1FC6; 0x1FC7; 0x1FCC; 0x1FCC; 0x1FD2; 0x1FD3; 0x1FD6; 0x1FD7; 0x1FE2; 0x1FE4; 0x1FE6; 0x1FE7;
      0x1FF2; 0x1FF4; 0x1FF6; 0x1FF7; 0x1FFC; 0x1FFC; 0xFB00; 0xFB06; 0xFB13; 0xFB17;
    |]

(* The Canonicalize function of ECMA-262 for case-insensitive matching. *)
let canonicalize r unicode_mode =
  if unicode_mode then
    (* Every character of a simple case folding class folds to the same character. *)
    Ucd.fold r
  else if
    (* A character beyond the Basic Multilingual Plane is two code units, and neither has an uppercase form. *)
    r > 0xFFFF || Charset.contains multi_upper r
  then r
  else begin
    let upper = Ucd.to_upper r in
    if (r >= 128 && upper < 128) || upper > 0xFFFF then r else upper
  end

(* The characters that are equivalent to some other character, each with all the members of its equivalence class:
   points sorted, class_of the number of each point's class in classes. *)
type table = { points : int array; class_of : int array; classes : int array array }

let make_table (classes : (int, int list) Hashtbl.t) =
  let points = Buffer_int.create 4096 and by_member = Hashtbl.create 4096 and all = ref [] and count = ref 0 in
  Hashtbl.iter
    (fun _ members ->
      let members = List.rev members in
      if List.length members >= 2 then begin
        List.iter
          (fun m ->
            Buffer_int.add points m;
            Hashtbl.replace by_member m !count)
          members;
        all := Array.of_list members :: !all;
        incr count
      end)
    classes;
  let points = Buffer_int.contents points in
  Array.sort compare points;
  let classes = Array.of_list (List.rev !all) in
  { points; class_of = Array.map (fun p -> Hashtbl.find by_member p) points; classes }

let unicode_fold_table =
  Once.make (fun () ->
      (* A class is a character that others fold to, with those others. *)
      let classes = Hashtbl.create 4096 in
      Array.iter
        (fun r ->
          let key = Ucd.fold r in
          let members = match Hashtbl.find_opt classes key with Some l -> l | None -> [ key ] in
          Hashtbl.replace classes key (r :: members))
        (Ucd.folding ());
      make_table classes)

let legacy_fold_table =
  Once.make (fun () ->
      let classes = Hashtbl.create 4096 in
      for r = 0 to 0xFFFF do
        let key = canonicalize r false in
        let members = match Hashtbl.find_opt classes key with Some l -> l | None -> [] in
        Hashtbl.replace classes key (r :: members)
      done;
      make_table classes)

(* The table of the grammar in use. It is built on first use, which only a case-insensitive pattern reaches. *)
let fold_classes unicode_mode = Once.get (if unicode_mode then unicode_fold_table else legacy_fold_table)

(* The first index of a sorted array whose element is at least x. *)
let lower_bound (a : int array) x =
  let lo = ref 0 and hi = ref (Array.length a) in
  while !lo < !hi do
    let mid = (!lo + !hi) lsr 1 in
    if a.(mid) < x then lo := mid + 1 else hi := mid
  done;
  !lo

(* The set of every character equivalent to some member of the set, and the number of characters with a case variant
   that the set holds, which is the work it did. *)
let fold_closure (set : Charset.t) unicode_mode =
  let table = fold_classes unicode_mode in
  let extra = Buffer_int.create 16 and variants = ref 0 in
  let r = set.ranges in
  for i = 0 to (Array.length r / 2) - 1 do
    let lo = r.(2 * i) and hi = r.((2 * i) + 1) in
    let k = ref (lower_bound table.points lo) in
    while !k < Array.length table.points && table.points.(!k) <= hi do
      incr variants;
      Array.iter
        (fun member ->
          if not (Charset.contains set member) then begin
            Buffer_int.add extra member;
            Buffer_int.add extra member
          end)
        table.classes.(table.class_of.(!k));
      incr k
    done
  done;
  if Buffer_int.length extra = 0 then (set, !variants)
  else begin
    Buffer_int.append_array extra set.ranges;
    (Charset.make (Buffer_int.contents extra), !variants)
  end

(* With the u flag and case-insensitive matching, \w and \b also count the two characters that fold to an ASCII letter
   (U+017F, the long s, and U+212A, the Kelvin sign) as word characters. *)
let fold_word_pairs = [| 0x30; 0x39; 0x41; 0x5A; 0x5F; 0x5F; 0x61; 0x7A; 0x17F; 0x17F; 0x212A; 0x212A |]

let fold_word_set = Charset.make fold_word_pairs
let not_fold_word_set = Charset.make ~negate:true fold_word_pairs
let is_fold_word_rune r = (r >= 0 && r < 128 && Charset.is_word_byte r) || r = 0x17F || r = 0x212A
