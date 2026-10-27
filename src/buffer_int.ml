(* A growable array of ints, as Rust's Vec<i32> is used in the crate: added to at the end, read and written by index,
   and copied out when done. *)

type t = { mutable data : int array; mutable len : int }

let create n = { data = Array.make (max n 4) 0; len = 0 }
let length b = b.len
let clear b = b.len <- 0

let get b k =
  if k < 0 || k >= b.len then invalid_arg "Buffer_int.get";
  Array.unsafe_get b.data k

let set b k v =
  if k < 0 || k >= b.len then invalid_arg "Buffer_int.set";
  Array.unsafe_set b.data k v

let grow b need =
  let n = ref (Array.length b.data) in
  while !n < need do
    n := 2 * !n
  done;
  let d = Array.make !n 0 in
  Array.blit b.data 0 d 0 b.len;
  b.data <- d

let add b v =
  if b.len = Array.length b.data then grow b (b.len + 1);
  Array.unsafe_set b.data b.len v;
  b.len <- b.len + 1

let truncate b n = if n < b.len then b.len <- max n 0

let pop b =
  if b.len = 0 then invalid_arg "Buffer_int.pop";
  b.len <- b.len - 1;
  Array.unsafe_get b.data b.len

let last b = get b (b.len - 1)
let contents b = Array.sub b.data 0 b.len
let append_array b a = Array.iter (add b) a
