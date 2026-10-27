(* The literals of a search, over UTF-8 and over UTF-16 (Literals of search.rs of the Rust crate, and the Object Pascal
   engine's Literals unit): a search for the leftmost occurrence of any of them, the first in the pattern's order of
   those that start there. And the sets of UTF-16 code units the searches look for (simd16.rs). A value of either is
   immutable once built.

   The UTF-8 searches take the text as a string and absolute offsets; the UTF-16 searches take a text (Units.text) and
   positions in its units. *)

(* The most units of a literal the packed search takes, so that the chunks' overlap stays short. *)
let packed_longest = 64

(* The most share of text (Finder.share16) a unit may have for the UTF-16 search to look for it alone. *)
let index_alone = 40

(* The most units packed into bytes at a time for the packed search, and the first chunk packed: each chunk after it
   is twice the one before, up to pack_chunk, so that a search whose occurrence is near packs little more than it
   reads. *)
let pack_chunk = 4096

let first_pack = 256

let is_surrogate u = u >= 0xD800 && u <= 0xDFFF

let utf8_bytes (r : int array) =
  let b = Buffer.create (Array.length r) in
  Array.iter (Compile.utf8_of_rune b) r;
  Buffer.contents b

let utf16_units (r : int array) =
  let out = ref [] in
  Array.iter
    (fun c ->
      if c > 0xFFFF then out := Units.low_surrogate c :: Units.high_surrogate c :: !out else out := c :: !out)
    r;
  Array.of_list (List.rev !out)

type t = {
  (* The search over UTF-8, and the one literal over UTF-8 when there is one (or ""). *)
  utf8 : Finder.t;
  single8 : string;
  (* The bytes the UTF-8 literals start with, as a set. *)
  firsts : string;
  (* The most code units of a literal. *)
  longest : int;
  (* Every literal over UTF-16, in the pattern's order. *)
  all16 : int array array;
  (* One literal over UTF-16 with a rare unit, one_unit, at one_at in it: the search for that unit, then a compare of
     the literal, when one16 is not empty. *)
  one16 : int array;
  one_unit : int;
  one_at : int;
  (* The rare unit up to four literals over UTF-16 all start with, or -1. *)
  shared16 : int;
  (* The search of the low bytes of several literals over UTF-16 (see find_packed), or None. *)
  packed16 : Finder.t option;
}

(* Builds the search for the literals, each a sequence of code points, with the most bytes of each written out over
   UTF-8 (see Search.utf8_cuts). *)
let make (lits : int array array) (cuts : int array) =
  (* A literal cut to fewer bytes finds the same starts, and one that an earlier literal begins with finds no start the
     earlier one does not. *)
  let u8 = ref [] in
  let all16 = Array.map utf16_units lits in
  let longest = Array.fold_left (fun m u -> max m (Array.length u)) 0 all16 in
  Array.iteri
    (fun k lit ->
      let b = utf8_bytes lit in
      (* A cut inside a character is not made: measured in the Rust crate, the Cyrillic letters with a three-byte form
         cut so made the search 1.2 times slower. *)
      let cut = cuts.(k) in
      let b = if cut < String.length b && Char.code b.[cut] land 0xC0 <> 0x80 then String.sub b 0 cut else b in
      let covered =
        List.exists (fun e -> String.length b >= String.length e && String.sub b 0 (String.length e) = e) !u8
      in
      if not covered then u8 := !u8 @ [ b ])
    lits;
  let u8 = Array.of_list !u8 in
  let firsts = Finder.set_of_bytes (Array.to_list (Array.map (fun l -> Char.code l.[0]) u8)) in
  let utf8 = Finder.build u8 Finder.byte_rank in
  let single8 = if Array.length u8 = 1 then u8.(0) else "" in
  let one16, one_unit, one_at =
    if Array.length all16 = 1 then begin
      let s = all16.(0) in
      let at = ref (-1) in
      Array.iteri
        (fun k u -> if (not (is_surrogate u)) && (!at < 0 || Finder.share16 u < Finder.share16 s.(!at)) then at := k)
        s;
      if !at >= 0 && Finder.share16 s.(!at) <= index_alone then (s, s.(!at), !at) else ([||], 0, 0)
    end
    else ([||], 0, 0)
  in
  let first = all16.(0).(0) in
  let shared =
    Array.length all16 > 1
    && Array.length all16 <= 4
    && (not (is_surrogate first))
    && Finder.share16 first <= index_alone
    && Array.for_all (fun u -> u.(0) = first) all16
  in
  let packed16 =
    if longest < packed_longest then begin
      (* The low byte of each unit, as the packed text holds it. *)
      let low = ref [] in
      Array.iter
        (fun u ->
          let bl = String.init (Array.length u) (fun j -> Char.chr (u.(j) land 0xFF)) in
          if not (List.mem bl !low) then low := !low @ [ bl ])
        all16;
      Some (Finder.build (Array.of_list !low) (Finder.packed_ranks all16))
    end
    else None
  in
  {
    utf8;
    single8;
    firsts;
    longest;
    all16;
    one16;
    one_unit;
    one_at;
    shared16 = (if shared then first else -1);
    packed16;
  }

(* The leftmost occurrence at or after from (an absolute offset) in UTF-8 text that ends at end_: where it starts, with
   its end in out.(0); or -1. *)
let find8 l s from end_ out = if from > end_ then -1 else Finder.find l.utf8 s from end_ out

(* Whether the units of a literal are at pos of UTF-16 text. *)
let has_units (t : Units.text) pos (lit : int array) =
  let n = Array.length lit in
  if pos < 0 || t.len - pos < n then false
  else begin
    let k = ref 0 in
    while !k < n && Units.Units16.get t (pos + !k) = Array.unsafe_get lit !k do
      incr k
    done;
    !k = n
  end

(* Where the first literal in order that starts at pos of UTF-16 text ends, or -1. *)
let literal_at16 l (t : Units.text) pos =
  let c = Units.Units16.get t pos in
  let n = Array.length l.all16 and k = ref 0 and found = ref (-1) in
  while !found < 0 && !k < n do
    let lit = l.all16.(!k) in
    if lit.(0) = c && has_units t pos lit then found := pos + Array.length lit;
    incr k
  done;
  !found

(* The leftmost occurrence at or after from of one of the literals (in the pattern's order, the first of those that
   start there): the low bytes of a chunk of units packed into bytes and searched for the low bytes of the literals,
   each candidate then compared with the literals themselves (a unit beyond ASCII packs to a byte an ASCII unit has
   too). An occurrence that starts so near the end of a chunk that a literal could start before it and end beyond the
   chunk is looked for again in the next chunk, which starts that far back. The first chunk is small, and each after it
   twice the one before up to the most, so that a search packs at most about twice what it reads. buf is the caller's
   buffer of at least pack_chunk bytes. *)
let find_packed l packed (t : Units.text) from (buf : Bytes.t) out =
  if from > t.len then -1
  else begin
    let overlap = max 0 (l.longest - 1) in
    let size = ref first_pack and at = ref from in
    let end_ = ref (min t.len (from + !size)) in
    let found = ref (-1) and fin = ref false in
    let packed_text = Bytes.unsafe_to_string buf in
    while not !fin do
      let n = !end_ - !at in
      ignore (Kernels.pack_low t.s t.base !at n buf);
      let last = !end_ = t.len in
      let limit = if last then n else n - overlap in
      let k = ref 0 in
      while !found < 0 && !k < n do
        let c = Finder.find packed packed_text !k n out in
        if c < 0 || c >= limit then k := n
        else begin
          let start = !at + c in
          let e = literal_at16 l t start in
          if e >= 0 then begin
            found := start;
            out.(0) <- e
          end
          else k := c + 1
        end
      done;
      if !found >= 0 || last then fin := true
      else begin
        at := !end_ - overlap;
        size := min pack_chunk (2 * !size);
        end_ := min t.len (!at + !size)
      end
    done;
    !found
  end

(* The leftmost occurrence at or after from in UTF-16 text: where it starts, with its end in out.(0); or -1. buf is the
   buffer of the packed search. *)
let find16 l (t : Units.text) from buf out =
  let from = max 0 from in
  let found = ref (-1) in
  if Array.length l.one16 > 0 then begin
    let at = ref (from + l.one_at) and fin = ref false in
    while not !fin do
      let q = Kernels.index_unit16 t.s t.base !at t.len l.one_unit in
      if q < 0 then fin := true
      else begin
        let start = q - l.one_at in
        if has_units t start l.one16 then begin
          found := start;
          out.(0) <- start + Array.length l.one16;
          fin := true
        end
        else at := q + 1
      end
    done;
    !found
  end
  else if l.shared16 >= 0 then begin
    let at = ref from and fin = ref false in
    while not !fin do
      let q = Kernels.index_unit16 t.s t.base !at t.len l.shared16 in
      if q < 0 then fin := true
      else begin
        let e = literal_at16 l t q in
        if e >= 0 then begin
          found := q;
          out.(0) <- e;
          fin := true
        end
        else at := q + 1
      end
    done;
    !found
  end
  else
    match l.packed16 with
    | Some p -> find_packed l p t from buf out
    | None ->
        let q = ref from in
        while !found < 0 && !q < t.len do
          let e = literal_at16 l t !q in
          if e >= 0 then begin
            found := !q;
            out.(0) <- e
          end
          else incr q
        done;
        !found

(* The leftmost occurrence that starts at or after from and at or before last, in UTF-8 text ending at end_ (absolute
   offsets), or -1. The few bytes between are looked at one at a time for a literal's first byte before a search is
   called, since a call on so short a text costs more than reading it. *)
let find_within8 l s end_ from last out =
  let last = min last (end_ - 1) in
  if from > last then -1
  else if l.single8 <> "" then begin
    let b = l.single8.[0] and q = ref from and found = ref (-1) in
    while !found < 0 && !q <= last do
      if String.unsafe_get s !q = b && Finder.starts_with s !q end_ l.single8 then found := !q;
      incr q
    done;
    !found
  end
  else begin
    let q = ref from in
    while !q <= last && not (Finder.in_set l.firsts (Char.code (String.unsafe_get s !q))) do
      incr q
    done;
    if !q > last then -1
    else begin
      let reach = min end_ (last + (3 * l.longest)) in
      let c = Finder.find l.utf8 s !q reach out in
      if c >= 0 && c <= last then c else -1
    end
  end

(* The leftmost occurrence that starts at or after from and at or before last, in UTF-16 text, or -1: each start
   between is looked at, as they are few. *)
let find_within16 l (t : Units.text) from last =
  let last = min last (t.len - 1) in
  let q = ref from and found = ref (-1) in
  while !found < 0 && !q <= last do
    if literal_at16 l t !q >= 0 then found := !q;
    incr q
  done;
  !found

(* A set of code units as ranges (inclusive pairs), for the units a match can start with and for the units that end a
   run of a loop: one unit by the search for it, one range by a compare, and any other set by a table of bits a unit at
   a time: a block of 256 bits for each high byte of a unit, the blocks the set has nothing in shared, so that a test is
   two loads and a shift. *)
type unit_set = {
  ranges : int array;
  (* The one unit of a set of one, or -1. *)
  one : int;
  (* For a set of more than one range, the block of bits of each high byte: blocks from 32 * block_of.(high byte). The
     high bytes the set has nothing in share block 0, which is empty. *)
  has_blocks : bool;
  block_of : int array;
  blocks : string;
}

let unit_set (ranges : int array) =
  let one = if Array.length ranges = 2 && ranges.(0) = ranges.(1) then ranges.(0) else -1 in
  if Array.length ranges > 2 then begin
    let all = Array.init 256 (fun _ -> Bytes.make 32 '\000') in
    let k = ref 0 in
    while !k < Array.length ranges do
      for u = ranges.(!k) to min 0xFFFF ranges.(!k + 1) do
        let b = all.(u lsr 8) and bit = u land 0xFF in
        Bytes.set b (bit lsr 3) (Char.chr (Char.code (Bytes.get b (bit lsr 3)) lor (1 lsl (bit land 7))))
      done;
      k := !k + 2
    done;
    (* Each distinct block is kept once (most high bytes of a large set are all in it or all out of it), block 0 being
       the empty one. *)
    let distinct = ref [ Bytes.make 32 '\000' ] in
    let block_of =
      Array.map
        (fun b ->
          let rec index k = function [] -> None | x :: rest -> if Bytes.equal x b then Some k else index (k + 1) rest in
          match index 0 !distinct with
          | Some k -> k
          | None ->
              distinct := !distinct @ [ b ];
              List.length !distinct - 1)
        all
    in
    { ranges; one; has_blocks = true; block_of; blocks = Bytes.to_string (Bytes.concat Bytes.empty !distinct) }
  end
  else { ranges; one; has_blocks = false; block_of = [||]; blocks = "" }

let[@inline] unit_in s u =
  if s.has_blocks then
    let bit = u land 0xFF in
    Char.code (String.unsafe_get s.blocks ((32 * s.block_of.(u lsr 8)) + (bit lsr 3))) land (1 lsl (bit land 7)) <> 0
  else u >= s.ranges.(0) && u <= s.ranges.(1)

(* The first unit at or after from of UTF-16 text in the set, or -1. *)
let unit_find s (t : Units.text) from =
  if s.one >= 0 then Kernels.index_unit16 t.s t.base from t.len s.one
  else begin
    let at = ref from and found = ref (-1) in
    while !found < 0 && !at < t.len do
      if unit_in s (Units.Units16.get t !at) then found := !at else incr at
    done;
    !found
  end
