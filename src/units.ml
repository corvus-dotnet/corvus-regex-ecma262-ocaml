(* Reading UTF-8 and UTF-16 text, and matching by UTF-16 code unit over UTF-8 text (units.rs of the Rust crate).

   ECMA-262 matches a pattern that has no u flag a code unit at a time. Over UTF-8, a character of one, two or three
   bytes is one code unit, and a character of four bytes is two, a high surrogate and a low one, so the matcher has to
   be able to stand between them. That position is written as the offset of the character's third byte. Each half
   then spans two bytes, every position is still an offset into the text, a group is still a pair of offsets, and no
   UTF-16 copy of the text is made. The third byte of a character is a continuation byte, which is no position at all
   when matching by code point, so the two readings cannot be confused.

   A text is a range of a string: len units from base (bytes of UTF-8, or 16-bit code units of UTF-16LE, two bytes
   each). Positions are 0 to len, in units. A character and its width are returned packed in one int, the code point
   shifted left by three and the width in the low three bits, so that reading one allocates nothing on any compiler
   (a pair would be a block, which only flambda removes). *)

type text = { s : string; base : int; len : int }

(* What a byte that is not UTF-8 decodes to, one byte at a time. *)
let rune_error = 0xFFFD

(* The first byte value that is not a character by itself. *)
let rune_self = 0x80

let[@inline] pack r w = (r lsl 3) lor w
let[@inline] rune_of p = p lsr 3
let[@inline] width_of p = p land 7
let[@inline] continuation b = b land 0xC0 = 0x80

(* The two UTF-16 code units of a code point beyond the Basic Multilingual Plane. *)
let[@inline] high_surrogate c = 0xD800 + ((c - 0x10000) lsr 10)
let[@inline] low_surrogate c = 0xDC00 + ((c - 0x10000) land 0x3FF)
let[@inline] is_high u = u >= 0xD800 && u <= 0xDBFF
let[@inline] is_low u = u >= 0xDC00 && u <= 0xDFFF

(* Whether a code unit is one of the 63 ASCII word characters. *)
let[@inline] is_word_unit c = c < 0x80 && Charset.is_word_byte c

(* The code point at s.[at], before the byte at stop, and its width: (rune_error, 1) for bytes that are not UTF-8 and
   (rune_error, 0) for no bytes. (utf8.DecodeRune.) *)
let decode_bytes s at stop =
  if at >= stop then pack rune_error 0
  else begin
    let p0 = Char.code (String.get s at) in
    if p0 < rune_self then pack p0 1
    else begin
      let n = stop - at in
      let b k = Char.code (String.get s (at + k)) in
      if p0 >= 0xC2 && p0 <= 0xDF && n >= 2 && continuation (b 1) then pack (((p0 land 0x1F) lsl 6) lor (b 1 land 0x3F)) 2
      else if p0 >= 0xE0 && p0 <= 0xEF && n >= 3 && continuation (b 2) then begin
        let lo, hi = if p0 = 0xE0 then (0xA0, 0xBF) else if p0 = 0xED then (0x80, 0x9F) else (0x80, 0xBF) in
        let b1 = b 1 in
        if b1 >= lo && b1 <= hi then pack (((p0 land 0x0F) lsl 12) lor ((b1 land 0x3F) lsl 6) lor (b 2 land 0x3F)) 3
        else pack rune_error 1
      end
      else if p0 >= 0xF0 && p0 <= 0xF4 && n >= 4 && continuation (b 2) && continuation (b 3) then begin
        let lo, hi = if p0 = 0xF0 then (0x90, 0xBF) else if p0 = 0xF4 then (0x80, 0x8F) else (0x80, 0xBF) in
        let b1 = b 1 in
        if b1 >= lo && b1 <= hi then
          pack
            (((p0 land 0x07) lsl 18) lor ((b1 land 0x3F) lsl 12) lor ((b 2 land 0x3F) lsl 6) lor (b 3 land 0x3F))
            4
        else pack rune_error 1
      end
      else pack rune_error 1
    end
  end

(* The code point that ends at s.[stop - 1], after the byte at start, and its width. (utf8.DecodeLastRune.) *)
let decode_last_bytes s start stop =
  if stop <= start then pack rune_error 0
  else begin
    let last = Char.code (String.get s (stop - 1)) in
    if last < rune_self then pack last 1
    else begin
      (* A character is four bytes at most, so its first byte is one of the last four. *)
      let lim = max start (stop - 4) in
      let first = ref (stop - 1) in
      let fin = ref false in
      while (not !fin) && !first > lim do
        decr first;
        if not (continuation (Char.code (String.get s !first))) then fin := true
      done;
      let p = decode_bytes s !first stop in
      if !first + width_of p <> stop then pack rune_error 1 else p
    end
  end

(* Whether a string is UTF-8, and the offset of its first byte that is not, or -1. *)
let first_invalid_utf8 s =
  let n = String.length s in
  let at = ref 0 and bad = ref (-1) in
  while !bad < 0 && !at < n do
    let p = decode_bytes s !at n in
    if rune_of p = rune_error && width_of p = 1 then bad := !at else at := !at + width_of p
  done;
  !bad

(* UTF-8 text. *)
module Units8 = struct
  let utf8 = true

  let[@inline] get t k = Char.code (String.get t.s (t.base + k))

  (* The code point at the position and its width. *)
  let[@inline] decode t pos =
    let c = get t pos in
    if c < rune_self then pack c 1 else decode_bytes t.s (t.base + pos) (t.base + t.len)

  (* The code point that ends at the position, and its width. *)
  let[@inline] decode_last t pos =
    let c = get t (pos - 1) in
    if c < rune_self then pack c 1 else decode_last_bytes t.s t.base (t.base + pos)

  (* The code unit at the position and the number of bytes it spans. *)
  let unit_at t pos =
    let c = get t pos in
    if c < rune_self then pack c 1
    else if c >= 0xF0 then begin
      let p = decode_bytes t.s (t.base + pos) (t.base + t.len) in
      if width_of p = 4 then pack (high_surrogate (rune_of p)) 2 else pack rune_error 1
    end
    else if c < 0xC0 && pos >= 2 && get t (pos - 2) >= 0xF0 then begin
      (* Between the halves of the character that starts two bytes back. *)
      let p = decode_bytes t.s (t.base + pos - 2) (t.base + t.len) in
      if width_of p = 4 then pack (low_surrogate (rune_of p)) 2 else decode_bytes t.s (t.base + pos) (t.base + t.len)
    end
    else decode_bytes t.s (t.base + pos) (t.base + t.len)

  (* The code unit that ends at the position and the number of bytes it spans. *)
  let unit_before t pos =
    let p = decode_last_bytes t.s t.base (t.base + pos) in
    if width_of p = 4 then pack (low_surrogate (rune_of p)) 2
    else if width_of p = 1 && rune_of p = rune_error && pos >= 2 && t.len - pos >= 2 && get t (pos - 2) >= 0xF0
    then begin
      (* Between the halves of the character that starts two bytes back. *)
      let q = decode_bytes t.s (t.base + pos - 2) (t.base + t.len) in
      if width_of q = 4 then pack (high_surrogate (rune_of q)) 2 else p
    end
    else p

  (* The character at the position: a code unit with units set, and a code point otherwise. *)
  let[@inline] read t pos units = if units then unit_at t pos else decode t pos
  let[@inline] read_last t pos units = if units then unit_before t pos else decode_last t pos

  (* The width of the last character of [from, pos), read as it was read going forward from from. *)
  let give_back t from pos units =
    if get t (pos - 1) < rune_self then 1
    else if units then width_of (unit_before t pos)
    else width_of (decode_last_bytes t.s (t.base + from) (t.base + pos))

  let line_terminator_at t pos =
    match get t pos with
    | 0x0A | 0x0D -> true
    | 0xE2 -> pos + 2 < t.len && get t (pos + 1) = 0x80 && (get t (pos + 2) = 0xA8 || get t (pos + 2) = 0xA9)
    | _ -> false

  (* U+2028 and U+2029 are E2 80 A8 and E2 80 A9. *)
  let line_terminator_before t pos =
    match get t (pos - 1) with
    | 0x0A | 0x0D -> true
    | 0xA8 | 0xA9 -> pos >= 3 && get t (pos - 3) = 0xE2 && get t (pos - 2) = 0x80
    | _ -> false

  (* Whether the units [from, to), compared equal to a group's, are the same code points: always over UTF-8. *)
  let whole _ _ _ = true

  (* Whether the position is the one between the halves of a character of four bytes: its third byte. *)
  let inside_four t pos =
    pos >= 2
    && pos < t.len
    && continuation (get t pos)
    && get t (pos - 2) >= 0xF0
    && width_of (decode_bytes t.s (t.base + pos - 2) (t.base + t.len)) = 4

  (* Appends [from, to) to the buffer. A bound between the halves of a character (which only a pattern without the u
     flag can make) cannot be written as UTF-8, and the half character is written as U+FFFD, as a lossy conversion of
     the UTF-16 text would write it; so the result is UTF-8 when the input is. *)
  let push_lossy b t from to_ =
    if from < to_ then begin
      let from = ref from and to_ = ref to_ in
      if inside_four t !from then begin
        Buffer.add_string b "\xEF\xBF\xBD";
        from := !from + 2
      end;
      let tail = !to_ < t.len && inside_four t !to_ in
      if tail then to_ := !to_ - 2;
      if !from < !to_ then Buffer.add_substring b t.s (t.base + !from) (!to_ - !from);
      if tail then Buffer.add_string b "\xEF\xBF\xBD"
    end

  (* The position a search from start begins at. With the u flag a position inside a character means the character;
     without it every code unit is a position, which is a character's first byte or the third byte of a character of
     four. *)
  let normalize t start unicode =
    if start = 0 || start >= t.len || not (continuation (get t start)) then start
    else begin
      let lead = ref start in
      while !lead > 0 && continuation (get t !lead) do
        decr lead
      done;
      if (not unicode) && get t !lead >= 0xF0 && start = !lead + 2 then start else !lead
    end

  (* The position after the character at pos (AdvanceStringIndex): a code point with the u flag and a code unit
     without it. *)
  let advance t pos unicode =
    if pos >= t.len then pos + 1
    else if unicode then pos + width_of (decode_bytes t.s (t.base + pos) (t.base + t.len))
    else pos + width_of (unit_at t pos)

  (* Whether the n units at a and at b are the same. *)
  let equal t a b n =
    let s = t.s and a = t.base + a and b = t.base + b in
    let k = ref 0 in
    while !k < n && String.unsafe_get s (a + !k) = String.unsafe_get s (b + !k) do
      incr k
    done;
    !k = n

  (* The literal's units in this encoding: lit is the UTF-8 form and lit16 the UTF-16LE form. *)
  let[@inline] lit_of lit _ = lit

  (* The number of units of a literal in this encoding. *)
  let[@inline] lit_units lit = String.length lit

  (* Whether the literal (in this encoding) is at the position, before the end of the text. *)
  let lit_at t pos lit =
    let n = String.length lit in
    if t.len - pos < n then false
    else begin
      let s = t.s and at = t.base + pos in
      let k = ref 0 in
      while !k < n && String.unsafe_get s (at + !k) = String.unsafe_get lit !k do
        incr k
      done;
      !k = n
    end
end

(* UTF-16 text, which may hold lone surrogates: code units of UTF-16LE, two bytes each. *)
module Units16 = struct
  let utf8 = false

  let[@inline] get t k = String.get_uint16_le t.s (t.base + (2 * k))

  let[@inline] decode t pos =
    let u = get t pos in
    if is_high u && pos + 1 < t.len then begin
      let v = get t (pos + 1) in
      if is_low v then pack (0x10000 + ((u - 0xD800) lsl 10) + (v - 0xDC00)) 2 else pack u 1
    end
    else pack u 1

  let[@inline] decode_last t pos =
    let u = get t (pos - 1) in
    if is_low u && pos >= 2 then begin
      let h = get t (pos - 2) in
      if is_high h then pack (0x10000 + ((h - 0xD800) lsl 10) + (u - 0xDC00)) 2 else pack u 1
    end
    else pack u 1

  let[@inline] unit_at t pos = pack (get t pos) 1
  let[@inline] unit_before t pos = pack (get t (pos - 1)) 1
  let[@inline] read t pos units = if units then unit_at t pos else decode t pos
  let[@inline] read_last t pos units = if units then unit_before t pos else decode_last t pos

  let give_back t from pos units =
    if (not units) && is_low (get t (pos - 1)) && pos >= from + 2 && is_high (get t (pos - 2)) then 2 else 1

  let line_terminator_at t pos = match get t pos with 0x0A | 0x0D | 0x2028 | 0x2029 -> true | _ -> false
  let line_terminator_before t pos = match get t (pos - 1) with 0x0A | 0x0D | 0x2028 | 0x2029 -> true | _ -> false

  (* False only when the comparison ends inside a surrogate pair of the text, which the u flag reads as one code
     point. *)
  let whole t from to_ =
    let starts_inside = from > 0 && from < t.len && is_low (get t from) && is_high (get t (from - 1)) in
    let ends_inside = to_ > from && to_ < t.len && is_high (get t (to_ - 1)) && is_low (get t to_) in
    not (starts_inside || ends_inside)

  let push_lossy b t from to_ = if from < to_ then Buffer.add_substring b t.s (t.base + (2 * from)) (2 * (to_ - from))

  let normalize t start unicode =
    if unicode && start > 0 && start < t.len && is_low (get t start) && is_high (get t (start - 1)) then start - 1
    else start

  let advance t pos unicode =
    if pos >= t.len then pos + 1 else if unicode then pos + width_of (decode t pos) else pos + 1

  let equal t a b n =
    let s = t.s and a = t.base + (2 * a) and b = t.base + (2 * b) and n = 2 * n in
    let k = ref 0 in
    while !k < n && String.unsafe_get s (a + !k) = String.unsafe_get s (b + !k) do
      incr k
    done;
    !k = n

  let[@inline] lit_of _ lit16 = lit16
  let[@inline] lit_units lit = String.length lit / 2

  let lit_at t pos lit =
    let n = String.length lit in
    if 2 * (t.len - pos) < n then false
    else begin
      let s = t.s and at = t.base + (2 * pos) in
      let k = ref 0 in
      while !k < n && String.unsafe_get s (at + !k) = String.unsafe_get lit !k do
        incr k
      done;
      !k = n
    end
end
