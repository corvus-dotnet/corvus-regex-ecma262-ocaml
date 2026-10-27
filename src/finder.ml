(* The search for any of several literals of bytes, as the Rust crate searches with memchr::memmem and aho-corasick
   (search.rs there), and as the Object Pascal engine's Finder unit does, whose rules this follows. One literal is found
   by one byte (OneFinder), by its byte rarest in text (RareFinder) or by two of its positions (PairFinder); several by
   the bytes they start with (FirstsFinder), by two positions (PairFinder) or by Teddy and a table (TableFinder). A
   finder is immutable once built, and safe to share between threads and domains.

   Every search takes the text as a string and the end of what it may read, and returns where the first literal found
   starts, in the pattern's order of those that start there, writing where it ends into out.(0); or -1. *)

(* min and max on ints: the library's are polymorphic, and without flambda each call compares through compare_val. *)
let[@inline] min (a : int) b = if a <= b then a else b
let[@inline] max (a : int) b = if a >= b then a else b

(* ByteRank is how common each byte is in text, from 0 (rare) to 255: the table memchr::memmem ranks a needle's bytes by
   (memchr 2.8, arch/all/packedpair/default_rank.rs; MIT or Unlicense), which pair8.rs takes. *)
let byte_rank =
  [|
    55; 52; 51; 50; 49; 48; 47; 46; 45; 103; 242; 66; 67; 229; 44; 43; 42; 41; 40; 39; 38; 37; 36; 35; 34; 33; 56; 32;
    31; 30; 29; 28; 255; 148; 164; 149; 136; 160; 155; 173; 221; 222; 134; 122; 232; 202; 215; 224; 208; 220; 204; 187;
    183; 179; 177; 168; 178; 200; 226; 195; 154; 184; 174; 126; 120; 191; 157; 194; 170; 189; 162; 161; 150; 193; 142;
    137; 171; 176; 185; 167; 186; 112; 175; 192; 188; 156; 140; 143; 123; 133; 128; 147; 138; 146; 114; 223; 151; 249;
    216; 238; 236; 253; 227; 218; 230; 247; 135; 180; 241; 233; 246; 244; 231; 139; 245; 243; 251; 235; 201; 196; 240;
    214; 152; 182; 205; 181; 127; 27; 212; 211; 210; 213; 228; 197; 169; 159; 131; 172; 105; 80; 98; 96; 97; 81; 207;
    145; 116; 115; 144; 130; 153; 121; 107; 132; 109; 110; 124; 111; 82; 108; 118; 141; 113; 129; 119; 125; 165; 117;
    92; 106; 83; 72; 99; 93; 65; 79; 166; 237; 163; 199; 190; 225; 209; 203; 198; 217; 219; 206; 234; 248; 158; 239;
    255; 255; 255; 255; 255; 255; 255; 255; 255; 255; 255; 255; 255; 255; 255; 255; 255; 255; 255; 255; 255; 255; 255;
    255; 255; 255; 255; 255; 255; 255; 255; 255; 255; 255; 255; 255; 255; 255; 255; 255; 255; 255; 255; 255; 255; 255;
    255; 255; 255; 255; 255; 255; 255; 255; 255; 255; 255; 255; 255; 255; 255; 255; 255; 255;
  |]

(* The rank under which a byte is searched for alone (the capital letters, the digits and the rarest lower case
   letters): a byte this rare pays for the compares where it is found. *)
let alone_rank = 200

(* The most tests of a position of the pair search. *)
let pair_most_tests = 4

(* The most bytes of the literals looked at before they are compared one by one. *)
let pair_checked = 16

(* The least share of text (unit_share) of the rare byte of one literal searched for with a second byte. *)
let pair_share = 25

(* Each ASCII unit's share of English text in parts per ten thousand: the small letters by their frequency (the table
   of the .NET reference engine's needle), the capitals as measured over the Industry texts, the space and the commonest
   punctuation, and ten for any other. *)
let unit_shares =
  let t = Array.make 128 10 in
  let lower = "etaoinshrdlcumwfgypbvkjxqz"
  and lower_per_mille =
    [| 102; 73; 66; 60; 56; 54; 50; 49; 48; 34; 32; 22; 22; 19; 19; 18; 16; 16; 15; 12; 8; 6; 2; 2; 1; 1 |]
  and upper = "ITHASWMECBNOYRLDPGFJUVKXQZ"
  and upper_per_ten_thousand =
    [| 65; 28; 21; 18; 16; 13; 13; 12; 10; 10; 9; 9; 8; 8; 7; 6; 6; 5; 5; 4; 2; 2; 2; 1; 1; 1 |]
  in
  String.iteri (fun k c -> t.(Char.code c) <- lower_per_mille.(k) * 10) lower;
  String.iteri (fun k c -> t.(Char.code c) <- upper_per_ten_thousand.(k)) upper;
  t.(Char.code ' ') <- 1700;
  t.(10) <- 200;
  t.(13) <- 200;
  t.(Char.code '.') <- 70;
  t.(Char.code ',') <- 80;
  t

(* An ASCII unit's share of English text in parts per ten thousand. *)
let unit_share b = unit_shares.(b land 0x7F)

let lu =
  Once.make (fun () -> match Ucd.category "Lu" with Some r -> Charset.make (Ucd.pairs r) | None -> Charset.make [||])

let lower_letters =
  Once.make (fun () ->
      let pairs n = Option.map Ucd.pairs (Ucd.category n) in
      Charset.make (Array.concat (List.filter_map pairs [ "Ll"; "Lt"; "Lm"; "Lo" ])))

(* A UTF-16 code unit's share of text, for the choice of the unit a search over UTF-16 looks for: an ASCII unit as
   unit_share has it; a capital of another alphabet (Latin, Greek, Cyrillic, Armenian) as rare as an ASCII capital and
   its small letters as common as the commonest ASCII ones, since a text in that script is mostly them; anything else
   rare. *)
let share16 u =
  if u < 0x80 then unit_shares.(u)
  else if u < 0x250 || (u >= 0x370 && u < 0x590) then
    if Charset.contains (Once.get lu) u then 20 else if Charset.contains (Once.get lower_letters) u then 500 else 10
  else 10

(* Whether a byte is rare enough in text for a search for it alone to pay: not a lower case letter (but the rarest
   five), a space or the commonest punctuation, nor a byte beyond ASCII, where the lead bytes of a script are common. *)
let rare_byte b =
  if (b >= Char.code 'A' && b <= Char.code 'Z') || (b >= Char.code '0' && b <= Char.code '9') then true
  else if b > 0x20 && b < 0x7F && not (b >= Char.code 'a' && b <= Char.code 'z') then
    not (String.contains ",.'\"-" (Char.chr b))
  else String.contains "jqxzk" (Char.chr b)

(* Whether a byte is rare enough to be searched for alone: one rare_byte names, and one the ranks do not mark common (a
   byte a UTF-16 literal's unit beyond ASCII packs to, which packed_ranks marks). *)
let alone (rank : int array) b = b < 0x80 && rare_byte b && rank.(b) <= alone_rank

let byte s k = Char.code (String.unsafe_get s k)

(* The position of the byte of a literal with the least share of text of those rare enough to be searched for alone,
   or -1 for none. *)
let rarest_alone l rank =
  let best = ref (-1) in
  for k = 0 to String.length l - 1 do
    if alone rank (byte l k) && (!best < 0 || unit_share (byte l k) < unit_share (byte l !best)) then best := k
  done;
  !best

(* Whether the byte of a UTF-8 literal at k ends a character of two bytes that is a capital of the alphabets share16
   counts (Latin, Greek, Cyrillic, Armenian), so that it is as rare as an ASCII capital in text: the last byte of a Han
   character, which thousands of characters share, is not, nor is a mark such as the middle dot of a Chinese name. *)
let capital_end l k =
  if k = 0 || byte l k < 0x80 || byte l k > 0xBF || byte l (k - 1) < 0xC2 || byte l (k - 1) > 0xDF then false
  else if k >= 2 && byte l (k - 2) >= 0xC0 then false
  else begin
    let u = ((byte l (k - 1) land 0x1F) lsl 6) lor (byte l k land 0x3F) in
    (u < 0x250 || (u >= 0x370 && u < 0x590)) && Charset.contains (Once.get lu) u
  end

let shortest_of (literals : string array) = Array.fold_left (fun m l -> min m (String.length l)) max_int literals

(* The values a position of the literals holds, sorted and each once. *)
let position_values (literals : string array) k =
  let seen = Array.make 256 false in
  Array.iter (fun l -> seen.(byte l k) <- true) literals;
  let out = ref [] in
  for b = 255 downto 0 do
    if seen.(b) then out := b :: !out
  done;
  Array.of_list !out

(* Whether the len bytes of lit are at s.[at], before end_. *)
let starts_with s at end_ lit =
  let len = String.length lit in
  if at < 0 || end_ - at < len then false
  else begin
    if len < 16 || String.unsafe_get s at <> String.unsafe_get lit 0
       || String.unsafe_get s (at + 1) <> String.unsafe_get lit 1 then begin
      let k = ref 0 in
      while !k < len && String.unsafe_get s (at + !k) = String.unsafe_get lit !k do
        incr k
      done;
      !k = len
    end
    else begin
      (* A literal of sixteen bytes or more whose first two bytes agree, eight bytes at a time from its third (an
         unboxed load on a 64-bit compiler), then the rest a byte at a time. Most candidates differ at the first or
         second byte, which a load of eight would cost more than, and a shorter literal (Huckleberry, 11 bytes) gained
         nothing from it in an A/B and lost 5%; Шерлок Холмс (23 bytes) goes from 52 us to 38 us. *)
      let k = ref 2 and same = ref true in
      while !same && !k + 8 <= len do
        if Int64.equal (String.get_int64_ne s (at + !k)) (String.get_int64_ne lit !k) then k := !k + 8
        else same := false
      done;
      while !same && !k < len do
        if String.unsafe_get s (at + !k) = String.unsafe_get lit !k then incr k else same := false
      done;
      !same
    end
  end

(* A set of bytes as 256 booleans in a string, a byte '\001' for a member. *)
let set_of_bytes bytes =
  let b = Bytes.make 256 '\000' in
  List.iter (fun x -> Bytes.set b x '\001') bytes;
  Bytes.to_string b

let[@inline] in_set set x = String.unsafe_get set x <> '\000'

(* The tests of a position's values as pairs of a mask and a value, a byte x passing one where x or mask = value: two
   values that differ in one bit (the cases of an ASCII letter, the lead bytes of the cases of a Cyrillic letter) make
   one test with that bit set, any other value a test of its own. None where that makes more than most. *)
let position_tests (values : int array) most =
  let n = Array.length values in
  let used = Array.make n false and tests = ref [] and count = ref 0 and over = ref false in
  for a = 0 to n - 1 do
    if not used.(a) then begin
      used.(a) <- true;
      let mask = ref 0 in
      (try
         for b = a + 1 to n - 1 do
           let bit = values.(a) lxor values.(b) in
           if (not used.(b)) && bit land (bit - 1) = 0 && bit <> 0 then begin
             used.(b) <- true;
             mask := bit;
             raise Exit
           end
         done
       with Exit -> ());
      if !count = most then over := true;
      tests := (values.(a) lor !mask) :: !mask :: !tests;
      incr count
    end
  done;
  if !over then None else Some (Array.of_list (List.rev !tests))

let disjoint (a : int array) (b : int array) = not (Array.exists (fun x -> Array.mem x b) a)

type t =
  | One of int
  | Rare of { lit : string; k : int; k2 : int }
  | Firsts of {
      fk : int;
      (* The number of values a, b, c and d (the last repeated), or -1 for the range lo to hi. *)
      count : int;
      a : int;
      b : int;
      c : int;
      d : int;
      (* The range lo to hi as Kernels.index_ranges reads it. *)
      franges : string;
      (* The literals by their byte at fk, in the pattern's order. *)
      by_value : string array array;
    }
  | Pair of {
      literals : string array;
      shortest : int;
      i : int;
      j : int;
      (* The tests of i, then of j, each a mask and a value. *)
      tests : string;
      ni : int;
      nj : int;
      (* For each of the first bytes of the literals up to pair_checked, the values they hold there. *)
      sets : string array;
      (* The literals are every sequence of the values sets holds, all of one length (a word's cases written out): a
         candidate whose bytes pass sets is then a literal, with nothing more to compare. *)
      every : bool;
    }
  | Table of {
      tk : int;
      at : string;
      tshortest : int;
      (* The literals by their first two bytes (two_bytes, where every literal has two), hashed into 1024 lists, or by
         their first byte, in the pattern's order: a candidate is compared with its list alone. By the first byte alone,
         every Cyrillic literal would share the lead byte D0 or D1. *)
      by_first : string array array;
      two_bytes : bool;
      (* For each of the first bytes of the literals (up to pair_checked), the values they hold there. *)
      tsets : string array;
      (* The nibble tables of the literals' first three bytes as Teddy reads them, or "" without a byte shuffle. *)
      tab : string;
    }

let bytes_of_ints a = String.init (Array.length a) (fun k -> Char.chr a.(k))

let build_firsts (literals : string array) (rank : int array) =
  let best_k = ref (-1) and best_range = ref false and best_cost = ref max_int in
  for k = 0 to shortest_of literals - 1 do
    let v = position_values literals k in
    (* Up to four values are tested for exactly; more as the range from the least to the greatest, where every byte in
       it is rare (the capital letters of a list of names), which is one test where the values would be as many. *)
    let is_range = Array.length v > 4 in
    let cost = ref 0 and rare = ref true in
    if is_range then
      for x = v.(0) to v.(Array.length v - 1) do
        rare := !rare && alone rank x;
        cost := !cost + (1 lsl (rank.(x) / 16))
      done
    else
      Array.iter
        (fun x ->
          rare := !rare && alone rank x;
          cost := !cost + (1 lsl (rank.(x) / 16)))
        v;
    (* Exact values before a range, then the rarer. *)
    cost := !cost + if is_range then 2 lsl 20 else 1 lsl 20;
    if !rare && !cost < !best_cost then begin
      best_k := k;
      best_range := is_range;
      best_cost := !cost
    end
  done;
  if !best_k < 0 then None
  else begin
    let k = !best_k in
    let v = position_values literals k in
    let by_value = Array.make 256 [||] in
    Array.iter
      (fun x -> by_value.(x) <- Array.of_list (List.filter (fun l -> byte l k = x) (Array.to_list literals)))
      v;
    let at idx = v.(min idx (Array.length v - 1)) in
    Some
      (if !best_range then
         let lo = v.(0) and hi = v.(Array.length v - 1) in
         Firsts { fk = k; count = -1; a = 0; b = 0; c = 0; d = 0; franges = bytes_of_ints [| lo; hi - lo |]; by_value }
       else Firsts { fk = k; count = Array.length v; a = at 0; b = at 1; c = at 2; d = at 3; franges = ""; by_value })
  end

(* The search for the literals (of at least two bytes each, in the pattern's order), or None where no two of their
   positions take pair_most_tests tests or fewer each. *)
let build_pair (literals : string array) (ranks : int array) =
  let shortest = shortest_of literals in
  if shortest < 2 then None
  else begin
    (* Each position of few enough tests, with how common its commonest value is. *)
    let values = Array.init shortest (position_values literals) in
    let tests = Array.map (fun v -> position_tests v pair_most_tests) values in
    let rank = Array.map (Array.fold_left (fun m x -> max m ranks.(x)) 0) values in
    (* The pair of the rarest positions, their values apart where any pair has them apart (a run of one byte would pass
       both otherwise), as PCRE2 chooses; of those as rare, the one of fewer tests. *)
    let bi = ref (-1) and bj = ref (-1) and best = ref max_int in
    List.iter
      (fun apart ->
        if !bi < 0 then
          for x = 0 to shortest - 1 do
            for y = x + 1 to shortest - 1 do
              match (tests.(x), tests.(y)) with
              | Some tx, Some ty when (not apart) || disjoint values.(x) values.(y) ->
                  let cost = ((rank.(x) + rank.(y)) lsl 8) lor (Array.length tx + Array.length ty) in
                  if cost < !best then begin
                    best := cost;
                    bi := x;
                    bj := y
                  end
              | _ -> ()
            done
          done)
      [ true; false ];
    if !bi < 0 then None
    else begin
      let checked = min shortest pair_checked in
      let sets =
        Array.init checked (fun k -> set_of_bytes (Array.to_list (Array.map (fun l -> byte l k) literals)))
      in
      let every = ref (shortest <= pair_checked && Array.for_all (fun l -> String.length l = shortest) literals) in
      let product = ref 1 and k = ref 0 in
      while !k < shortest && !every do
        product := !product * Array.length values.(!k);
        every := !product <= Array.length literals;
        incr k
      done;
      let every = !every && !product = Array.length literals in
      let ti = Option.get tests.(!bi) and tj = Option.get tests.(!bj) in
      Some
        (Pair
           {
             literals;
             shortest;
             i = !bi;
             j = !bj;
             tests = bytes_of_ints (Array.append ti tj);
             ni = Array.length ti / 2;
             nj = Array.length tj / 2;
             sets;
             every;
           })
    end
  end

(* The tables of Teddy for literals of shortest bytes or more. Each literal is in one of eight buckets by its first
   bytes (up to three, and up to shortest) with the ASCII letters folded, so that the cases of a word share a bucket and
   add no starts to the others; the buckets of the distinct prefixes are taken in turn. A position at or past shortest
   passes every byte. *)
let teddy_tables (literals : string array) shortest =
  let t = Bytes.make 96 '\000' in
  let width = min shortest 3 in
  for m = width to 2 do
    Bytes.fill t (32 * m) 32 '\xFF'
  done;
  let keys = ref [] in
  Array.iter
    (fun l ->
      let key =
        String.init width (fun m ->
            let b = byte l m in
            Char.chr (if (b lor 0x20) >= Char.code 'a' && (b lor 0x20) <= Char.code 'z' then b lor 0x20 else b))
      in
      let rec index k = function [] -> None | x :: rest -> if x = key then Some k else index (k + 1) rest in
      let bucket =
        match index 0 (List.rev !keys) with
        | Some k -> k
        | None ->
            keys := key :: !keys;
            List.length !keys - 1
      in
      let bit = 1 lsl (bucket land 7) in
      for m = 0 to width - 1 do
        let b = byte l m in
        let lo = (32 * m) + (b land 15) and hi = (32 * m) + 16 + (b lsr 4) in
        Bytes.set t lo (Char.chr (Char.code (Bytes.get t lo) lor bit));
        Bytes.set t hi (Char.chr (Char.code (Bytes.get t hi) lor bit))
      done)
    literals;
  Bytes.to_string t

(* The list of a table finder's literals that a candidate whose first two bytes are b0 and b1 is compared with. *)
let[@inline] table_key b0 b1 = ((b0 lsl 3) lxor b1) land 1023

(* The list a candidate at q is compared with. A candidate starts at least tshortest bytes before the end, so its second
   byte is there when two_bytes. A function of its own, not a closure, so that a search makes none. *)
let[@inline] table_list (by_first : string array array) two_bytes s q =
  if two_bytes then by_first.(table_key (byte s q) (byte s (q + 1))) else by_first.(byte s q)

let build_table (literals : string array) (rank : int array) =
  let shortest = shortest_of literals in
  let best_k = ref 0 and best_cost = ref max_int in
  for k = 0 to shortest - 1 do
    let cost = Array.fold_left (fun acc x -> acc + (1 lsl (rank.(x) / 16))) 0 (position_values literals k) in
    if cost < !best_cost then begin
      best_cost := cost;
      best_k := k
    end
  done;
  let k = !best_k in
  let two_bytes = shortest >= 2 in
  let size = if two_bytes then 1024 else 256 in
  let key l = if two_bytes then table_key (byte l 0) (byte l 1) else byte l 0 in
  let by_first = Array.make size [||] in
  for b = 0 to size - 1 do
    by_first.(b) <- Array.of_list (List.filter (fun l -> key l = b) (Array.to_list literals))
  done;
  let nsets = min shortest pair_checked in
  Table
    {
      tk = k;
      at = set_of_bytes (Array.to_list (Array.map (fun l -> byte l k) literals));
      tshortest = shortest;
      by_first;
      two_bytes;
      tsets = Array.init nsets (fun j -> set_of_bytes (Array.to_list (Array.map (fun l -> byte l j) literals)));
      tab = (if !Kernels.has_shuffle then teddy_tables literals shortest else "");
    }

(* The search for the literals, in the pattern's order, with how common each byte is (byte_rank over UTF-8, and the
   ranks of packed_ranks for the low bytes of UTF-16 units). *)
let build (literals : string array) (rank : int array) =
  if Array.length literals = 1 then begin
    let l = literals.(0) in
    if String.length l = 1 then One (byte l 0)
    else begin
      (* The byte with the least share of text of those rare enough to be searched for alone; the pair search where
         every byte is common. *)
      let best = rarest_alone l rank in
      if best >= 0 then begin
        (* A byte as common as a capital T leaves many starts that are not the literal, which a second byte turns away
           in the same pass; a rarer one is searched for alone. A literal of three bytes or fewer is a word as common
           as its capital (nearly every T of English text begins The), so a second byte turns few starts away. *)
        let k2 = ref (-1) in
        if !Kernels.has_wide && unit_share (byte l best) >= pair_share && String.length l > 3 then
          for k = 0 to String.length l - 1 do
            if byte l k <> byte l best && (!k2 < 0 || rank.(byte l k) < rank.(byte l !k2)) then k2 := k
          done;
        Rare { lit = l; k = best; k2 = !k2 }
      end
      else begin
        (* With no ASCII byte rare enough alone, where the processor reads 32 bytes at a time: the last byte of a
           capital of another alphabet, which only that capital ends, searched for alone (the Ш of Шерлок); otherwise
           the two rarest bytes (by rank) tested together, as memchr's memmem does. *)
        let wide =
          if !Kernels.has_wide then begin
            let cap = ref (-1) in
            for k = 0 to String.length l - 1 do
              if capital_end l k && (!cap < 0 || rank.(byte l k) < rank.(byte l !cap)) then cap := k
            done;
            if !cap >= 0 then Some (Rare { lit = l; k = !cap; k2 = -1 })
            else begin
              let b = ref 0 in
              for k = 1 to String.length l - 1 do
                if rank.(byte l k) < rank.(byte l !b) then b := k
              done;
              let k2 = ref (-1) in
              for k = 0 to String.length l - 1 do
                if byte l k <> byte l !b && (!k2 < 0 || rank.(byte l k) < rank.(byte l !k2)) then k2 := k
              done;
              if !k2 >= 0 then Some (Rare { lit = l; k = !b; k2 = !k2 }) else None
            end
          end
          else None
        in
        match wide with
        | Some f -> f
        | None -> ( match build_pair literals rank with Some p -> p | None -> build_table literals rank)
      end
    end
  end
  else
    match build_firsts literals rank with
    (* Literals whose first bytes are a range (the capitals of a list of names) go to Teddy where there is a byte
       shuffle: every capital of the text is a candidate of the range, and Teddy's three bytes turn nearly all of them
       away (Sherlock|Holmes|Watson|Irene|Adler|John|Baker, 239 us to 71 us; V8 223 us). *)
    | Some (Firsts { count = -1; _ }) when !Kernels.has_shuffle && shortest_of literals >= 3 -> build_table literals rank
    | Some f -> f
    | None -> (
        (* Several literals with no rare first bytes go to Teddy where the processor has a byte shuffle, as the crate
           takes them to aho-corasick's Teddy: three positions in one pass turn away more starts than the pair search's
           two. The cases of one word stay with the pair search, whose candidates need no comparing (every) or whose
           positions take one test each. *)
        match build_pair literals rank with
        | Some (Pair p as pair) when p.every || (p.ni = 1 && p.nj = 1) || not !Kernels.has_shuffle -> pair
        | Some pair when not !Kernels.has_shuffle -> pair
        | _ -> build_table literals rank)

(* Where the first literal of a list that starts at q ends, or -1. *)
let first_literal s q end_ (lits : string array) =
  let n = Array.length lits and k = ref 0 and found = ref (-1) in
  while !found < 0 && !k < n do
    let l = Array.unsafe_get lits !k in
    if starts_with s q end_ l then found := q + String.length l;
    incr k
  done;
  !found

(* Where the literal of a pair or table finder that starts at q ends, testing the bytes of its first positions first,
   or -1. *)
let literal_at_sets s q end_ (sets : string array) every shortest (lits : string array) =
  let n = min (Array.length sets) (end_ - q) in
  let k = ref 0 in
  while !k < n && in_set (Array.unsafe_get sets !k) (byte s (q + !k)) do
    incr k
  done;
  if !k < n then -1 else if every then q + shortest else first_literal s q end_ lits

(* The first literal at or after from, before end_: where it starts, with its end in out.(0); or -1. *)
let find f s from end_ (out : int array) =
  match f with
  | One b ->
      let q = Kernels.index_byte1 s from end_ b in
      if q >= 0 then out.(0) <- q + 1;
      q
  | Rare { lit; k; k2 } ->
      let len = String.length lit and found = ref (-1) in
      let start = ref from in
      if k2 >= 0 then begin
        (* The two bytes tested together: each candidate is compared with the literal. *)
        let last = end_ - len and b1 = byte lit k and b2 = byte lit k2 in
        while !found < 0 && !start <= last do
          let q = Kernels.eq2_find s !start last end_ k k2 b1 b2 in
          if q < 0 then start := last + 1
          else if starts_with s q end_ lit then found := q
          else start := q + 1
        done
      end
      else begin
        let at = ref (from + k) and b = byte lit k in
        while !found < 0 && !at < end_ do
          let q = Kernels.index_byte1 s !at end_ b in
          if q < 0 then at := end_
          else if starts_with s (q - k) end_ lit then found := q - k
          else at := q + 1
        done
      end;
      if !found >= 0 then out.(0) <- !found + len;
      !found
  | Firsts { fk; count; a; b; c; d; franges; by_value } ->
      let at = ref (from + fk) and found = ref (-1) in
      while !found < 0 && !at < end_ do
        let q =
          match count with
          | 1 -> Kernels.index_byte1 s !at end_ a
          | 2 -> Kernels.index_byte2 s !at end_ a b
          | 3 | 4 -> Kernels.index_byte4 s !at end_ (Kernels.pack4 a b c d)
          | _ -> Kernels.index_ranges s !at end_ franges 1
        in
        if q < 0 then at := end_
        else begin
          (* A byte of the range no literal holds there has no literals. *)
          let e = first_literal s (q - fk) end_ by_value.(byte s q) in
          if e >= 0 then begin
            found := q - fk;
            out.(0) <- e
          end
          else at := q + 1
        end
      done;
      !found
  | Pair { literals; shortest; i; j; tests; ni; nj; sets; every } ->
      let last = end_ - shortest and found = ref (-1) and at = ref from in
      while !found < 0 && !at <= last do
        let q = Kernels.pair_find s !at last end_ i j tests ni nj in
        if q < 0 then at := last + 1
        else begin
          let e = literal_at_sets s q end_ sets every shortest literals in
          if e >= 0 then begin
            found := q;
            out.(0) <- e
          end
          else at := q + 1
        end
      done;
      !found
  | Table { tk; at; tshortest; by_first; two_bytes; tsets; tab } ->
      let last = end_ - tshortest and found = ref (-1) and q = ref from in
      if tab <> "" then
        (* Teddy: each candidate is compared with the literals of its first byte. *)
        while !found < 0 && !q <= last do
          let c = Kernels.teddy_find s !q last end_ tab in
          if c < 0 then q := last + 1
          else begin
            let e = literal_at_sets s c end_ tsets false tshortest (table_list by_first two_bytes s c) in
            if e >= 0 then begin
              found := c;
              out.(0) <- e
            end
            else q := c + 1
          end
        done
      else
        (* The table of the bytes at the rarest position, read a byte at a time. *)
        while !found < 0 && !q <= last do
          if in_set at (byte s (!q + tk)) then begin
            let e = literal_at_sets s !q end_ tsets false tshortest (table_list by_first two_bytes s !q) in
            if e >= 0 then begin
              found := !q;
              out.(0) <- e
            end
            else incr q
          end
          else incr q
        done;
      !found

(* How common each low byte of the units of UTF-16 literals is in the packed text: an ASCII unit's byte as byte_rank
   has it, and a byte any of the literals' units beyond ASCII packs to as common (a Cyrillic letter in Russian text) or
   nearly so (a Han character, whose low byte any of thousands share), so that neither is searched for alone or taken
   for a rare position. *)
let packed_ranks (literals : int array array) =
  let r = Array.copy byte_rank in
  Array.iter
    (Array.iter (fun u ->
         if u >= 0x80 then begin
           let b = u land 0xFF in
           let v = if u >= 0x400 && u <= 0x4FF then 245 else 230 in
           if v > r.(b) then r.(b) <- v
         end))
    literals;
  r
