(* The search kernels, in C (cre_stubs.c): each is an external that allocates nothing, with untagged ints, so a call is
   a plain C call. None takes more than five arguments, which every ABI passes in registers: opam-repository's CI showed
   a sixth, passed on the stack on s390x, arriving wrong there, so where a kernel needs more its small ints are packed into
   one by a wrapper here. Every position is an offset into the string, and each reads only below n, which the caller has
   checked lies within the string. *)

(* The first position in [from, n) whose byte is b, or -1. *)
external index_byte1 : string -> (int[@untagged]) -> (int[@untagged]) -> (int[@untagged]) -> (int[@untagged])
  = "cre_index_byte1_byte" "cre_index_byte1"
[@@noalloc]

(* The first position in [from, n) whose byte is a or b, or -1. *)
external index_byte2 :
  string -> (int[@untagged]) -> (int[@untagged]) -> (int[@untagged]) -> (int[@untagged]) -> (int[@untagged])
  = "cre_index_byte2_byte" "cre_index_byte2"
[@@noalloc]

(* The first position in [from, n) whose byte is one of four (which may repeat), packed into one int by pack4, or -1:
   four arguments, which a call passes in registers. *)
external index_byte4 : string -> (int[@untagged]) -> (int[@untagged]) -> (int[@untagged]) -> (int[@untagged])
  = "cre_index_byte4_byte" "cre_index_byte4"
[@@noalloc]

let[@inline] pack4 a b c d = a lor (b lsl 8) lor (c lsl 16) lor (d lsl 24)

(* The first position in [from, n) whose byte is in one of nr ranges, given as a least byte and a width each, or -1. *)
external index_ranges : string -> (int[@untagged]) -> (int[@untagged]) -> string -> (int[@untagged]) -> (int[@untagged])
  = "cre_index_ranges_byte" "cre_index_ranges"
[@@noalloc]

(* The first position in [from, n) of a run of len bytes each in one of nr ranges, or -1. *)
external index_run_c :
  string -> (int[@untagged]) -> (int[@untagged]) -> string -> (int[@untagged]) -> (int[@untagged])
  = "cre_index_run_byte" "cre_index_run"
[@@noalloc]

let[@inline] index_run s from n ranges nr len = index_run_c s from n ranges (nr lor (len lsl 8))

(* The first position in [from, n) whose byte is not an ASCII member of a set given as its nibble bitmap
   (Charset.t.nibbles), or n: the end of a run of the set's ASCII members, a byte beyond ASCII ending it too. *)
external ascii_run_end : string -> (int[@untagged]) -> (int[@untagged]) -> string -> (int[@untagged])
  = "cre_ascii_run_end_byte" "cre_ascii_run_end"
[@@noalloc]

(* ascii_run_end over UTF-16LE text from the byte base: the first unit at or after from and before n that is not an
   ASCII member of the set, or n. *)
external ascii_run_end16 :
  string -> (int[@untagged]) -> (int[@untagged]) -> (int[@untagged]) -> string -> (int[@untagged])
  = "cre_ascii_run_end16_byte" "cre_ascii_run_end16"
[@@noalloc]

(* The last position in [lo, hi) whose byte is one of count (one to three) bytes of back, or -1. *)
external last_of : string -> (int[@untagged]) -> (int[@untagged]) -> string -> (int[@untagged]) -> (int[@untagged])
  = "cre_last_of_byte" "cre_last_of"
[@@noalloc]

(* The first start in [at, last] whose byte at i passes one of ni tests and at j one of nj, each a mask and a value,
   the bytes before n; or -1. *)
external pair_find_c :
  string -> (int[@untagged]) -> (int[@untagged]) -> (int[@untagged]) -> string -> (int[@untagged])
  = "cre_pair_find_byte" "cre_pair_find"
[@@noalloc]

(* The positions (each below 65,536), the numbers of tests and the shortest literal's length (n - last) packed into one
   int, so that the call passes five arguments. *)
let[@inline] pair_find s at last n i j tests ni nj =
  pair_find_c s at n (i lor (j lsl 16) lor (ni lsl 32) lor (nj lsl 36) lor ((n - last) lsl 40)) tests

(* The first start in [at, last] whose byte at i is a and at j is b, the bytes before n; or -1. *)
external eq2_find_c :
  string -> (int[@untagged]) -> (int[@untagged]) -> (int[@untagged]) -> (int[@untagged]) -> (int[@untagged])
  = "cre_eq2_find_byte" "cre_eq2_find"
[@@noalloc]

(* The positions (each below 65,536) and the two bytes packed into one int, so that the call passes five arguments. *)
let[@inline] eq2_find s at last n i j a b = eq2_find_c s at last n (i lor (j lsl 16) lor (a lsl 32) lor (b lsl 40))

(* The first start in [at, last] that passes Teddy's nibble tables (96 bytes), the bytes before n; or -1. *)
external teddy_find :
  string -> (int[@untagged]) -> (int[@untagged]) -> (int[@untagged]) -> string -> (int[@untagged])
  = "cre_teddy_find_byte" "cre_teddy_find"
[@@noalloc]

(* The first unit at or after from and before n of UTF-16LE text from the byte base that is v, or -1. *)
external index_unit16 :
  string -> (int[@untagged]) -> (int[@untagged]) -> (int[@untagged]) -> (int[@untagged]) -> (int[@untagged])
  = "cre_index_unit16_byte" "cre_index_unit16"
[@@noalloc]

(* The first unit at or after from and before n of UTF-16LE text from the byte base that is in one of nr ranges (1 to 8),
   given as 16-bit little-endian pairs of a least unit and a width, or -1. *)
external index_units16_c :
  string -> (int[@untagged]) -> (int[@untagged]) -> (int[@untagged]) -> string -> (int[@untagged])
  = "cre_index_units16_byte" "cre_index_units16"
[@@noalloc]

(* The number of ranges is the length of the string of ranges over four, so that the call passes five arguments. *)
let[@inline] index_units16 s base from n ranges (_ : int) = index_units16_c s base from n ranges

(* Writes the low byte of count units of UTF-16LE text from unit from (from the byte base) into dst. *)
external pack_low : string -> (int[@untagged]) -> (int[@untagged]) -> (int[@untagged]) -> Bytes.t -> (int[@untagged])
  = "cre_pack_low_byte" "cre_pack_low"
[@@noalloc]

external has_shuffle_c : unit -> (int[@untagged]) = "cre_has_shuffle_byte" "cre_has_shuffle" [@@noalloc]
external has_wide_c : unit -> (int[@untagged]) = "cre_has_wide_byte" "cre_has_wide" [@@noalloc]

(* Whether the processor has a byte shuffle (SSSE3, or any AArch64), which Teddy reads its tables with. The tests turn
   it off to try the table finder's own loop. *)
let has_shuffle = ref (has_shuffle_c () <> 0)

(* Whether the processor reads 32 bytes at a time (AVX2), where a literal's two rarest bytes searched for together
   pay. *)
let has_wide = ref (has_wide_c () <> 0)
