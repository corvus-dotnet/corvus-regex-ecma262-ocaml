(* Runs the engine over the ECMA-262 regular expression test data (github.com/corvus-dotnet/ecma262-regex-test-data), as
   the runner of the Rust crate does (tools/src/bin/dataset.rs there), with the same judging rules.

     run <data directory> <wrong-cases output file> [file filter] [utf16|utf8|both] [--compliance <file>]

   Each record is asked about under each grammar V8 found it valid in: the record's flags with u added (the unicode
   answers), the record's flags alone (the legacy answers) and, in the file that has them, the flags with v added (the
   unicode_sets answers). A text is matched as UTF-16 and as UTF-8; a text with a lone surrogate has no UTF-8 form and
   is matched as UTF-16 alone. Records marked v8_deviation are left out. A pattern is UTF-8, so a lone surrogate in one
   is read as U+FFFD, as the Rust runner reads it. captures.jsonl is judged on exec, the global search, the sticky flag,
   replace and split. ill-formed-utf8.jsonl holds bytes that are not UTF-8, each one answer over UTF-8 under each
   grammar the pattern is valid in, whose right answer is a refusal (Invalid_argument).

   exec/test262.jsonl, the exec calls of test262 (in a directory of its own), is judged last: each call one exec with
   the record's flags from its lastIndex, right when the positions of the match and of every group, the named groups
   and, with g or y, the lastIndex after are V8's; one answer over UTF-16 and, when the text holds no lone surrogate and
   lastIndex is not between the halves of a pair, one over UTF-8 with positions in bytes.

   With --compliance <file>, the engine's compliance object is written to the file, as the benchmark page reads it
   (docs/implementing-a-source.md in corvus-regex-ecma262-benchmarks). *)

module R = Corvus_regex_ecma262

(* UTF-8 and UTF-16 helpers of the runner's own. *)
module Units = struct
  let is_high u = u >= 0xD800 && u <= 0xDBFF
  let is_low u = u >= 0xDC00 && u <= 0xDFFF
  let high_surrogate c = 0xD800 + ((c - 0x10000) lsr 10)
  let low_surrogate c = 0xDC00 + ((c - 0x10000) land 0x3FF)

  (* The code point at s.[at] and its width, (0xFFFD, 1) for a byte that is not UTF-8. *)
  let decode s at =
    let n = String.length s in
    let d = String.get_utf_8_uchar s at in
    ignore n;
    if Uchar.utf_decode_is_valid d then (Uchar.to_int (Uchar.utf_decode_uchar d), Uchar.utf_decode_length d)
    else (0xFFFD, max 1 (Uchar.utf_decode_length d))
end

module Compile = struct
  let utf8_of_rune b r = Buffer.add_utf_8_uchar b (Uchar.of_int r)
end


let limit_seconds = 2.0
let replacement = "<$&|$1|$2|$10|$<a>|$$|$`|$'>"

(* The file of the exec calls of test262, in a directory of its own. *)
let exec_file = "exec/test262.jsonl"

type counts = {
  mutable right : int;
  mutable wrong : int;
  mutable refused : int;
  mutable accepted : int;
  mutable limited : int;
  mutable errors : int;
}

let counts () = { right = 0; wrong = 0; refused = 0; accepted = 0; limited = 0; errors = 0 }

let add c o =
  c.right <- c.right + o.right;
  c.wrong <- c.wrong + o.wrong;
  c.refused <- c.refused + o.refused;
  c.accepted <- c.accepted + o.accepted;
  c.limited <- c.limited + o.limited;
  c.errors <- c.errors + o.errors

let as_utf16 = ref true
let as_utf8 = ref true

(* A small JSON reader whose strings are UTF-16 code units (int arrays) and keep lone surrogates. *)
type json = Obj of (string * json) list | Arr of json list | Str of int array | Num of float | Bool of bool | Null

let utf16_of_utf8 s =
  let out = ref [] and k = ref 0 and n = String.length s in
  while !k < n do
    let r, w = Units.decode s !k in
    if r > 0xFFFF then out := Units.low_surrogate r :: Units.high_surrogate r :: !out else out := r :: !out;
    k := !k + w
  done;
  Array.of_list (List.rev !out)

(* The UTF-8 of UTF-16 units, each lone surrogate written as U+FFFD. *)
let lossy (s : int array) =
  let b = Buffer.create (Array.length s) in
  let n = Array.length s and k = ref 0 in
  while !k < n do
    let u = s.(!k) in
    if Units.is_high u && !k + 1 < n && Units.is_low s.(!k + 1) then begin
      Compile.utf8_of_rune b (0x10000 + ((u - 0xD800) lsl 10) + (s.(!k + 1) - 0xDC00));
      k := !k + 2
    end
    else begin
      Compile.utf8_of_rune b (if u >= 0xD800 && u <= 0xDFFF then 0xFFFD else u);
      incr k
    end
  done;
  Buffer.contents b

(* UTF-16 units as the UTF-16LE bytes the engine reads. *)
let le (s : int array) =
  let b = Bytes.create (2 * Array.length s) in
  Array.iteri (fun k u -> Bytes.set_uint16_le b (2 * k) u) s;
  Bytes.unsafe_to_string b

let units_of_le s = Array.init (String.length s / 2) (fun k -> String.get_uint16_le s (2 * k))

let parse_json line =
  let s = line and i = ref 0 in
  let n = String.length s in
  let space () =
    while !i < n && (s.[!i] = ' ' || s.[!i] = '\t' || s.[!i] = '\n' || s.[!i] = '\r') do
      incr i
    done
  in
  let expect c =
    if s.[!i] <> c then failwith (Printf.sprintf "expected %c at %d" c !i);
    incr i
  in
  let str () =
    expect '"';
    let out = ref [] in
    let fin = ref false in
    while not !fin do
      let c = s.[!i] in
      if c = '"' then begin
        incr i;
        fin := true
      end
      else if c <> '\\' then begin
        let start = !i in
        while !i < n && s.[!i] <> '"' && s.[!i] <> '\\' do
          incr i
        done;
        out := List.rev_append (Array.to_list (utf16_of_utf8 (String.sub s start (!i - start)))) !out
      end
      else begin
        let e = s.[!i + 1] in
        i := !i + 2;
        match e with
        | 'n' -> out := 0x0A :: !out
        | 't' -> out := 0x09 :: !out
        | 'r' -> out := 0x0D :: !out
        | 'b' -> out := 0x08 :: !out
        | 'f' -> out := 0x0C :: !out
        | 'u' ->
            out := int_of_string ("0x" ^ String.sub s !i 4) :: !out;
            i := !i + 4
        | c -> out := Char.code c :: !out
      end
    done;
    Array.of_list (List.rev !out)
  in
  let rec value () =
    space ();
    match s.[!i] with
    | '{' ->
        incr i;
        space ();
        if s.[!i] = '}' then begin
          incr i;
          Obj []
        end
        else begin
          let fields = ref [] and fin = ref false in
          while not !fin do
            space ();
            let key = lossy (str ()) in
            space ();
            expect ':';
            let v = value () in
            fields := (key, v) :: !fields;
            space ();
            if s.[!i] = ',' then incr i
            else begin
              expect '}';
              fin := true
            end
          done;
          Obj (List.rev !fields)
        end
    | '[' ->
        incr i;
        space ();
        if s.[!i] = ']' then begin
          incr i;
          Arr []
        end
        else begin
          let items = ref [] and fin = ref false in
          while not !fin do
            items := value () :: !items;
            space ();
            if s.[!i] = ',' then incr i
            else begin
              expect ']';
              fin := true
            end
          done;
          Arr (List.rev !items)
        end
    | '"' -> Str (str ())
    | 't' ->
        i := !i + 4;
        Bool true
    | 'f' ->
        i := !i + 5;
        Bool false
    | 'n' ->
        i := !i + 4;
        Null
    | _ ->
        let start = !i in
        while !i < n && String.contains "+-0123456789.eE" s.[!i] do
          incr i
        done;
        Num (float_of_string (String.sub s start (!i - start)))
  in
  let v = value () in
  v

let get o name = match o with Obj fields -> List.assoc_opt name fields | _ -> None
let str o = match o with Some (Str s) -> s | _ -> [||]
let array o = match o with Some (Arr a) -> Some a | _ -> None
let is_true o = o = Some (Bool true)

let has_lone_surrogate (s : int array) =
  let n = Array.length s and k = ref 0 and lone = ref false in
  while (not !lone) && !k < n do
    let u = s.(!k) in
    if Units.is_high u then
      if !k + 1 < n && Units.is_low s.(!k + 1) then k := !k + 2
      else lone := true
    else if Units.is_low u then lone := true
    else incr k
  done;
  !lone

let escape (units : int array) =
  let b = Buffer.create (Array.length units) in
  Array.iter
    (fun u ->
      if u >= 0x20 && u < 0x7F && u <> Char.code '\\' then Buffer.add_char b (Char.chr u)
      else Buffer.add_string b (Printf.sprintf "\\u%04x" u))
    units;
  Buffer.contents b

let first_line s = match String.index_opt s '\n' with Some k -> String.sub s 0 k | None -> s

type mode = { add_flag : string; answers : string; valid : json option }

(* The modes a record is asked about: the flags to add, the name of the answers, and whether V8 found it valid. *)
let modes r =
  let valid = get r "valid" in
  let v name = match valid with Some o -> get o name | None -> None in
  let ms = [ { add_flag = "u"; answers = "unicode"; valid = v "unicode" }; { add_flag = ""; answers = "legacy"; valid = v "legacy" } ] in
  match v "unicode_sets" with None -> ms | Some _ as x -> ms @ [ { add_flag = "v"; answers = "unicode_sets"; valid = x } ]

let compile pattern flags = R.compile ~flags ~match_timeout:limit_seconds pattern

(* The value of f (), or the exception it raised. *)
let attempt f = match f () with v -> Ok v | exception e -> Error e

let limit_kind = function R.Match_timeout -> Some "time" | R.Match_memory -> Some "memory" | _ -> None

let check name shown flags text encoding expected got c wrong =
  let where_ = Printf.sprintf "%s\t/%s/%s\t%s\t%s" name shown flags (escape text) encoding in
  match got with
  | Ok g when g = expected -> c.right <- c.right + 1
  | Ok _ ->
      c.wrong <- c.wrong + 1;
      wrong := Printf.sprintf "WRONG\t%s\texpected %b" where_ expected :: !wrong
  | Error e -> (
      match limit_kind e with
      | Some k ->
          c.limited <- c.limited + 1;
          wrong := Printf.sprintf "LIMIT\t%s\t%s" where_ k :: !wrong
      | None ->
          c.errors <- c.errors + 1;
          wrong := Printf.sprintf "ERROR\t%s\t%s" where_ (Printexc.to_string e) :: !wrong)

let judge_tests name r pattern c wrong =
  let flags = lossy (str (get r "flags")) in
  let shown = escape (utf16_of_utf8 pattern) in
  List.iter
    (fun m ->
      let mode_flags = flags ^ m.add_flag in
      let expected_valid = m.valid = Some (Bool true) in
      match compile pattern mode_flags with
      | Error e ->
          if expected_valid then begin
            c.refused <- c.refused + 1;
            wrong :=
              Printf.sprintf "REFUSED\t%s\t/%s/%s\t%s" name shown mode_flags (first_line (R.error_message e)) :: !wrong
          end
      | Ok re ->
          if not expected_valid then begin
            c.accepted <- c.accepted + 1;
            wrong := Printf.sprintf "ACCEPTED-INVALID\t%s\t/%s/%s" name shown mode_flags :: !wrong
          end
          else
            List.iter
              (fun t ->
                match get t m.answers with
                | Some (Bool expected) ->
                    let text = str (get t "text") in
                    if !as_utf16 then
                      check name shown mode_flags text "utf16" expected
                        (attempt (fun () -> R.is_match_utf16 re (le text)))
                        c wrong;
                    if !as_utf8 && not (has_lone_surrogate text) then
                      check name shown mode_flags text "utf8" expected
                        (attempt (fun () -> R.is_match re (lossy text)))
                        c wrong
                | _ -> ())
              (match array (get r "tests") with Some a -> a | None -> []))
    (modes r)

let judge_ill_formed r pattern c wrong =
  let name = "ill-formed-utf8.jsonl" in
  let flags = lossy (str (get r "flags")) in
  let shown = escape (utf16_of_utf8 pattern) in
  List.iter
    (fun m ->
      let mode_flags = flags ^ m.add_flag in
      let expected_valid = m.valid = Some (Bool true) in
      let head = Printf.sprintf "%s\t/%s/%s" name shown mode_flags in
      match compile pattern mode_flags with
      | Error e ->
          if expected_valid then begin
            c.refused <- c.refused + 1;
            wrong := Printf.sprintf "REFUSED\t%s\t%s" head (first_line (R.error_message e)) :: !wrong
          end
      | Ok re ->
          if not expected_valid then begin
            c.accepted <- c.accepted + 1;
            wrong := Printf.sprintf "ACCEPTED-INVALID\t%s" head :: !wrong
          end
          else
            List.iter
              (fun t ->
                let hex = lossy (str (get t "bytes")) in
                let bytes = String.init (String.length hex / 2) (fun k -> Char.chr (int_of_string ("0x" ^ String.sub hex (2 * k) 2))) in
                match R.is_match re bytes with
                | got ->
                    c.wrong <- c.wrong + 1;
                    wrong := Printf.sprintf "WRONG\t%s\t%s\tutf8\tread as UTF-8 (%b), not refused" head hex got :: !wrong
                | exception Invalid_argument _ -> c.right <- c.right + 1
                | exception e -> (
                    match limit_kind e with
                    | Some _ ->
                        c.limited <- c.limited + 1;
                        wrong := Printf.sprintf "LIMIT\t%s\t%s\tutf8" head hex :: !wrong
                    | None ->
                        c.errors <- c.errors + 1;
                        wrong := Printf.sprintf "ERROR\t%s\t%s\tutf8\t%s" head hex (Printexc.to_string e) :: !wrong))
              (match array (get r "tests") with Some a -> a | None -> []))
    (modes r)

(* For each UTF-16 index of a text (and the index past its end), the byte offset in UTF-8. *)
let byte_offsets (s : int array) =
  let n = Array.length s in
  let at = Array.make (n + 1) 0 in
  let b = ref 0 and k = ref 0 in
  while !k < n do
    at.(!k) <- !b;
    let c = s.(!k) in
    if Units.is_high c && !k + 1 < n && Units.is_low s.(!k + 1) then begin
      at.(!k + 1) <- !b + 2;
      b := !b + 4;
      k := !k + 2
    end
    else begin
      b := !b + (if c < 0x80 then 1 else if c < 0x800 then 2 else 3);
      incr k
    end
  done;
  at.(n) <- !b;
  at

(* The slots of the first match at or after start (in the units of the text), or None. *)
let submatch re text start utf16 =
  let caps = R.captures re in
  let found =
    if utf16 then R.exec_into_utf16 re caps ~pos:start text else R.exec_into re caps ~pos:start text
  in
  if found then Some (Array.init (2 * (R.group_count re + 1)) (fun k -> if k mod 2 = 0 then R.capture_start caps (k / 2) else R.capture_end caps (k / 2)))
  else None

(* The groups of an exec answer: None for no match, else the bounds of each group, None for one not taking part. *)
let exec_of e =
  match e with
  | Some (Arr groups) ->
      Some
        (List.map
           (function Arr [ Num a; Num b ] -> Some (int_of_float a, int_of_float b) | _ -> None)
           groups)
  | _ -> None

let groups_of = function
  | None -> None
  | Some slots ->
      Some
        (List.init (Array.length slots / 2) (fun g ->
             if slots.(2 * g) >= 0 then Some (slots.(2 * g), slots.((2 * g) + 1)) else None))

let same_slots expected got at =
  match (expected, got) with
  | None, None -> true
  | Some e, Some g ->
      List.length e = List.length g
      && List.for_all2
           (fun a b ->
             match (a, b) with
             | None, None -> true
             | Some (a0, a1), Some (b0, b1) -> at a0 = b0 && at a1 = b1
             | _ -> false)
           e g
  | _ -> false

(* Compares a global search with the data's, which keeps the first twenty matches only: found has at most 21, and one
   more than the data has is right only when the data kept twenty. *)
let same_all all found at =
  let la = List.length all and lf = List.length found in
  let rec go n a f =
    match f with
    | [] -> true
    | s :: f' -> if n >= la then la = 20 else same_slots (List.nth all n) (groups_of (Some s)) at && go (n + 1) a f'
  in
  go 0 all found && not ((la < 20 && lf <> la) || (la = 20 && lf < 20))

let same_all_bounds all found at =
  let la = List.length all and lf = List.length found in
  let rec go n f =
    match f with
    | [] -> true
    | (b0, b1) :: f' -> (
        if n >= la then la = 20
        else
          match List.nth all n with
          | Some (Some (e0, e1) :: _) -> at e0 = b0 && at e1 = b1 && go (n + 1) f'
          | _ -> false)
  in
  go 0 found && not ((la < 20 && lf <> la) || (la = 20 && lf < 20))

let take n seq =
  let rec go k s acc = if k = 0 then List.rev acc else match s () with Seq.Nil -> List.rev acc | Seq.Cons (x, s') -> go (k - 1) s' (x :: acc) in
  go n seq []

let match_slots m =
  Array.init (2 * (R.Match.group_count m + 1)) (fun k ->
      match R.Match.span m (k / 2) with Some (f, e) -> if k mod 2 = 0 then f else e | None -> -1)

let same_split16 expected got =
  match expected with
  | Some (Arr e) ->
      List.length e = List.length got
      && List.for_all2
           (fun e g -> match (e, g) with Null, None -> true | Str s, Some p -> units_of_le p = s | _ -> false)
           e got
  | _ -> false

let judge_captures r pattern c wrong =
  let flags = lossy (str (get r "flags")) in
  let valid = get r "valid" in
  let shown = escape (utf16_of_utf8 pattern) in
  let cache = Hashtbl.create 4 in
  let make f =
    match Hashtbl.find_opt cache f with
    | Some v -> v
    | None ->
        let v = match compile pattern f with Ok re -> Some re | Error _ -> None in
        Hashtbl.replace cache f v;
        v
  in
  List.iter
    (fun t ->
      if not (is_true (get t "v8_deviation")) then begin
        let text = str (get t "text") in
        List.iter
          (fun m ->
            let valid_here = match valid with Some o -> is_true (get o m) | None -> false in
            match get t m with
            | Some a when valid_here -> (
                let mode_flags = if m = "unicode" then flags ^ "u" else flags in
                let where_ = Printf.sprintf "/%s/%s\t%s" shown mode_flags (escape text) in
                match (make mode_flags, make (mode_flags ^ "g"), make (mode_flags ^ "y")) with
                | Some re, Some g, Some y ->
                    let lone = has_lone_surrogate text in
                    let t16 = le text in
                    let u8 = if lone then "" else lossy text in
                    let byte_at = if lone then [||] else byte_offsets text in
                    let to_byte k = byte_at.(k) in
                    let v what ok =
                      match ok with
                      | Error e when limit_kind e <> None ->
                          c.limited <- c.limited + 1;
                          wrong := Printf.sprintf "LIMIT\tcaptures.jsonl\t%s\t%s" where_ what :: !wrong
                      | Error e ->
                          c.errors <- c.errors + 1;
                          wrong := Printf.sprintf "ERROR\tcaptures.jsonl\t%s\t%s\t%s" where_ what (Printexc.to_string e) :: !wrong
                      | Ok true -> c.right <- c.right + 1
                      | Ok false ->
                          c.wrong <- c.wrong + 1;
                          wrong := Printf.sprintf "WRONG\tcaptures.jsonl\t%s\t%s" where_ what :: !wrong
                    in
                    (* exec *)
                    let expected = exec_of (get a "exec") in
                    (match attempt (fun () -> submatch re t16 0 true) with
                    | Error e when limit_kind e <> None ->
                        c.limited <- c.limited + 1;
                        wrong := Printf.sprintf "LIMIT\tcaptures.jsonl\t%s" where_ :: !wrong
                    | got ->
                        v "exec" (Result.map (fun got -> same_slots expected (groups_of got) Fun.id) got);
                        if not lone then
                          v "exec-utf8"
                            (attempt (fun () -> same_slots expected (groups_of (submatch re u8 0 false)) to_byte));
                        (* all *)
                        let all = List.map (fun e -> exec_of (Some e)) (match array (get a "all") with Some x -> x | None -> []) in
                        v "all"
                          (attempt (fun () -> same_all all (List.map match_slots (take 21 (R.matches_utf16 g t16))) Fun.id));
                        if not lone then
                          v "all-utf8"
                            (attempt (fun () ->
                                 same_all_bounds all
                                   (List.map (fun m -> (R.Match.start m, R.Match.end_ m)) (take 21 (R.matches g u8)))
                                   to_byte));
                        (* sticky at 1 *)
                        if Array.length text > 0 then begin
                          let sticky = exec_of (get a "sticky") in
                          v "sticky" (attempt (fun () -> same_slots sticky (groups_of (submatch y t16 1 true)) Fun.id));
                          if (not lone) && not (Units.is_high text.(0)) then
                            v "sticky-utf8"
                              (attempt (fun () -> same_slots sticky (groups_of (submatch y u8 byte_at.(1) false)) to_byte))
                        end;
                        (* replace *)
                        let replace1 = str (get a "replace") and replace_all = str (get a "replaceAll") in
                        let rep16 = le (utf16_of_utf8 replacement) in
                        v "replace" (attempt (fun () -> units_of_le (R.replace_utf16 re t16 rep16) = replace1));
                        v "replaceAll" (attempt (fun () -> units_of_le (R.replace_all_utf16 g t16 rep16) = replace_all));
                        if not lone then begin
                          (* Over UTF-8 a half character is written as U+FFFD, which is what a lossy conversion of V8's
                             answer writes for it. *)
                          v "replace-utf8" (attempt (fun () -> R.replace re u8 replacement = lossy replace1));
                          v "replaceAll-utf8" (attempt (fun () -> R.replace_all g u8 replacement = lossy replace_all))
                        end;
                        (* split *)
                        v "split" (attempt (fun () -> same_split16 (get a "split") (R.split_utf16 re t16)));
                        v "split3" (attempt (fun () -> same_split16 (get a "split3") (R.split_utf16 ~limit:3 re t16))))
                | _ ->
                    c.refused <- c.refused + 1;
                    wrong := Printf.sprintf "REFUSED\tcaptures.jsonl\t%s" where_ :: !wrong)
            | _ -> ())
          [ "unicode"; "legacy" ]
      end)
    (match array (get r "tests") with Some a -> a | None -> [])

(* exec/test262.jsonl *)

let same_span e (slots : int array) g at =
  let took = g >= 0 && slots.(2 * g) >= 0 in
  match e with
  | Arr [ Num a; Num b ] -> took && at (int_of_float a) = slots.(2 * g) && at (int_of_float b) = slots.((2 * g) + 1)
  | _ -> not took

let named_group names (slots : int array) name =
  match List.assoc_opt name names with
  | None -> -1
  | Some groups -> ( match List.find_opt (fun g -> slots.(2 * g) >= 0) groups with Some g -> g | None -> -1)

let same_exec answer names_groups slots carries at =
  match (answer, slots) with
  | (None | Some Null), None -> true
  | (None | Some Null), Some _ | Some _, None -> false
  | Some ans, Some slots -> (
      match get ans "captures" with
      | Some (Arr caps) when List.length caps = Array.length slots / 2 ->
          List.for_all Fun.id (List.mapi (fun g e -> same_span e slots g at) caps)
          && (match get ans "groups" with
             | Some (Obj groups) ->
                 List.length groups = List.length names_groups
                 && List.for_all
                      (fun (name, e) -> List.mem_assoc name names_groups && same_span e slots (named_group names_groups slots name) at)
                      groups
             | _ -> names_groups = [])
          && ((not carries) || match get ans "lastIndex" with Some (Num li) -> at (int_of_float li) = slots.(1) | _ -> false)
      | _ -> false)

let judge_exec r pattern c wrong =
  let flags = lossy (str (get r "flags")) in
  let shown = escape (utf16_of_utf8 pattern) in
  match compile pattern flags with
  | Error e ->
      c.refused <- c.refused + 1;
      wrong := Printf.sprintf "REFUSED\t%s\t/%s/%s\t%s" exec_file shown flags (first_line (R.error_message e)) :: !wrong
  | Ok re ->
      let carries = String.contains flags 'g' || String.contains flags 'y' in
      let names_groups = R.group_numbers re in
      List.iter
        (fun t ->
          if not (is_true (get t "v8_deviation")) then begin
            let text = str (get t "text") in
            let len = Array.length text in
            let li = match get t "lastIndex" with Some (Num v) -> v | _ -> 0. in
            let start = if carries then if li > float_of_int len then len + 1 else int_of_float li else 0 in
            let answer = get t "answer" in
            let shown_li = if Float.is_integer li && Float.abs li < 1e15 then string_of_int (int_of_float li) else string_of_float li in
            let where_ = Printf.sprintf "/%s/%s\t%s\t%s" shown flags (escape text) shown_li in
            let count encoding res =
              match res with
              | Error _ ->
                  c.limited <- c.limited + 1;
                  wrong := Printf.sprintf "LIMIT\t%s\t%s\t%s" exec_file where_ encoding :: !wrong
              | Ok true -> c.right <- c.right + 1
              | Ok false ->
                  c.wrong <- c.wrong + 1;
                  wrong := Printf.sprintf "WRONG\t%s\t%s\t%s" exec_file where_ encoding :: !wrong
            in
            if !as_utf16 then
              count "utf16"
                (attempt (fun () ->
                     let slots = if start <= len then submatch re (le text) start true else None in
                     same_exec answer names_groups slots carries Fun.id));
            let inside_pair = start > 0 && start < len && Units.is_low text.(start) && Units.is_high text.(start - 1) in
            if !as_utf8 && (not (has_lone_surrogate text)) && not inside_pair then begin
              let byte_at = byte_offsets text in
              count "utf8"
                (attempt (fun () ->
                     let slots = if start <= len then submatch re (lossy text) byte_at.(start) false else None in
                     same_exec answer names_groups slots carries (fun k -> byte_at.(k))))
            end
          end)
        (match array (get r "tests") with Some a -> a | None -> [])

let judge_record name r c wrong =
  let pattern = lossy (str (get r "pattern")) in
  if name = exec_file then judge_exec r pattern c wrong
  else if name = "captures.jsonl" then (if !as_utf16 then judge_captures r pattern c wrong)
  else if name = "ill-formed-utf8.jsonl" then (if !as_utf8 then judge_ill_formed r pattern c wrong)
  else judge_tests name r pattern c wrong

let write_compliance path t =
  let calls = ref [] in
  if !as_utf16 then
    calls :=
      "\"utf16\":[\"Corvus_regex_ecma262.is_match_utf16\",\"Corvus_regex_ecma262.exec_into_utf16\",\"Corvus_regex_ecma262.matches_utf16\",\"Corvus_regex_ecma262.replace_utf16\",\"Corvus_regex_ecma262.split_utf16\"]"
      :: !calls;
  if !as_utf8 then
    calls :=
      !calls
      @ [
          "\"utf8\":[\"Corvus_regex_ecma262.is_match\",\"Corvus_regex_ecma262.exec_into\",\"Corvus_regex_ecma262.matches\",\"Corvus_regex_ecma262.replace\"]";
        ];
  let oc = open_out_bin path in
  Printf.fprintf oc
    "{\"right\":%d,\"wrong\":%d,\"refused\":%d,\"acceptedInvalid\":%d,\"limited\":%d,\"judge\":{\"runner\":\"https://github.com/corvus-dotnet/corvus-regex-ecma262-ocaml/blob/main/tools/dataset/run.ml\",\"calls\":{%s}},\"excludes\":[]}\n"
    t.right t.wrong t.refused t.accepted (t.limited + t.errors) (String.concat "," !calls);
  close_out oc

let read_lines path =
  let ic = open_in_bin path in
  let lines = ref [] in
  (try
     while true do
       let l = input_line ic in
       if String.trim l <> "" then lines := l :: !lines
     done
   with End_of_file -> ());
  close_in ic;
  List.rev !lines

let () =
  let args = Array.to_list Sys.argv |> List.tl in
  let rec take_compliance acc = function
    | "--compliance" :: f :: rest -> (Some f, List.rev_append acc rest)
    | x :: rest -> take_compliance (x :: acc) rest
    | [] -> (None, List.rev acc)
  in
  let compliance, args = take_compliance [] args in
  match args with
  | directory :: wrong_path :: rest ->
      let filter = match rest with f :: _ -> f | [] -> "*.jsonl" in
      let encodings = match rest with _ :: e :: _ -> e | _ -> "both" in
      as_utf16 := encodings = "both" || encodings = "utf16";
      as_utf8 := encodings = "both" || encodings = "utf8";
      let glob = Str.regexp ("^" ^ Str.global_replace (Str.regexp_string "*") ".*" (Str.global_replace (Str.regexp_string ".") "\\." filter) ^ "$") in
      let files =
        Sys.readdir directory |> Array.to_list
        |> List.filter (fun f -> Str.string_match glob f 0 && Filename.check_suffix f ".jsonl" && not (Sys.is_directory (Filename.concat directory f)))
        |> List.sort compare
      in
      let files =
        if (filter = "*.jsonl" || String.length filter >= 4 && String.sub filter 0 4 = "exec") && Sys.file_exists (Filename.concat directory exec_file)
        then files @ [ exec_file ]
        else files
      in
      let total = counts () and wrong = ref [] and records = ref 0 and skipped = ref 0 in
      let clock = Unix.gettimeofday () in
      List.iter
        (fun name ->
          let c = counts () in
          List.iter
            (fun line ->
              let r = parse_json line in
              if is_true (get r "v8_deviation") then incr skipped
              else begin
                incr records;
                judge_record name r c wrong
              end)
            (read_lines (Filename.concat directory name));
          Printf.printf "%-24s right %10d  wrong %6d  refused-valid %4d  accepted-invalid %4d  limit %4d  %6.1fs\n%!" name
            c.right c.wrong c.refused c.accepted c.limited (Unix.gettimeofday () -. clock);
          add total c)
        files;
      let oc = open_out_bin wrong_path in
      List.iter (fun l -> output_string oc (l ^ "\n")) (List.sort compare !wrong);
      close_out oc;
      Option.iter (fun p -> write_compliance p total) compliance;
      Printf.printf
        "\nrecords %d (skipped %d deviations)  right %d  wrong %d  refused-valid %d  accepted-invalid %d  limit %d  errors %d  in %.1fs\n"
        !records !skipped total.right total.wrong total.refused total.accepted total.limited total.errors
        (Unix.gettimeofday () -. clock);
      if total.wrong + total.refused + total.accepted + total.errors + total.limited <> 0 then exit 1
  | _ ->
      prerr_endline "usage: run <data directory> <wrong-cases output file> [file filter] [utf16|utf8|both] [--compliance <file>]";
      exit 2
