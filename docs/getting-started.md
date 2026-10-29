# Getting started with OCaml

`corvus-regex-ecma262` is an OCaml engine, with no dependencies beyond the compiler's distribution, whose patterns
have the grammar and the answers of JavaScript's `RegExp`, held to V8's answers on the ECMA-262 test data, over UTF-8
and UTF-16. A compiled pattern is shared freely between threads (OCaml 4.14) and domains (OCaml 5).

## Install

```
opam install corvus-regex-ecma262
```

It needs OCaml 4.14 or later, 64-bit. In a dune project, add `corvus-regex-ecma262` to the `libraries` of a stanza.

## Compile a pattern

`compile` takes a pattern and its flags as `new RegExp(pattern, flags)` takes them: `d`, `g`, `i`, `m`, `s`, `u`, `v` and
`y`, and returns a `result`, so a pattern from outside is tried without an exception; `compile_exn` raises
`Compile_error`. Without `u` or `v` a pattern is read by the Annex B grammar and matched by UTF-16 code unit, as
ECMA-262 reads it; with them, by code point.

```ocaml
module R = Corvus_regex_ecma262

let () =
  let date = R.compile_exn ~flags:"gu" {|(?<year>\d{4})-(?<month>\d{2})|} in
  assert (R.group_count date = 2);
  assert (match R.compile ~flags:"u" "a{2,1}" with Error (R.Invalid _) -> true | _ -> false);
  assert (match R.compile ~flags:"uv" "a" with Error (R.Flags _) -> true | _ -> false);
  assert (not (R.is_valid ~flags:"u" "(?<n>a)(?<n>b)"))
```

## Match

Positions are 0-based byte offsets into the UTF-8 text. A match is a `Match.t`, with its groups by number and by name.

```ocaml
module R = Corvus_regex_ecma262

let () =
  let date = R.compile_exn ~flags:"gu" {|(?<year>\d{4})-(?<month>\d{2})|} in
  let text = "due 2026-10, then 2027-01" in
  assert (R.is_match date text);
  (match R.find date text with
   | Some m ->
       assert (R.Match.start m = 4 && R.Match.end_ m = 11 && R.Match.value m = "2026-10");
       assert (R.Match.named m "year" = Some "2026" && R.Match.group m 2 = Some "10")
   | None -> assert false);
  (* Every match of a global search, as matchAll makes it. *)
  assert (List.map R.Match.value (List.of_seq (R.matches date text)) = [ "2026-10"; "2027-01" ]);
  assert (R.count date text = 2);
  (* replace replaces every match with the g flag, and the first without it, with every $ form of ECMA-262. *)
  assert (R.replace date text "$<month>/$1" = "due 10/2026, then 01/2027");
  (* split splices in what the groups captured; a group that did not take part is None. *)
  let pieces = R.split (R.compile_exn {|\s*(;)\s*|}) "a ; b;c" in
  assert (pieces = [ Some "a"; Some ";"; Some "b"; Some ";"; Some "c" ])
```

## UTF-8 and UTF-16

A string is UTF-8, and is checked before it is read: text that is not UTF-8 raises `Invalid_argument`, and is never
matched or read as U+FFFD. `utf8_text` checks a text once, for a text matched many times. The `_utf16` functions take a
string of UTF-16LE code units, lone surrogates and all, with positions in code units.

```ocaml
module R = Corvus_regex_ecma262

(* UTF-16LE of UTF-8 text. *)
let utf16 s =
  let b = Buffer.create (2 * String.length s) in
  let rec go i =
    if i < String.length s then begin
      let d = String.get_utf_8_uchar s i in
      Buffer.add_utf_16le_uchar b (Uchar.utf_decode_uchar d);
      go (i + Uchar.utf_decode_length d)
    end
  in
  go 0;
  Buffer.contents b

let () =
  let dot_b = R.compile_exn ~flags:"u" ".b" in
  let span m = (R.Match.start m, R.Match.end_ m) in
  assert (Option.map span (R.find dot_b "a😀b") = Some (1, 6));            (* bytes of UTF-8 *)
  assert (Option.map span (R.find_utf16 dot_b (utf16 "a😀b")) = Some (1, 4)); (* UTF-16 code units *)
  assert (R.is_match_text dot_b (R.utf8_text_exn "a😀b"));
  assert (match R.is_match dot_b "\xff" with _ -> false | exception Invalid_argument _ -> true)
```

## Patterns and texts from outside

A match has no limit by default. A program that matches a pattern or a text it did not write gives the pattern a match
timeout in seconds, as a .NET `Regex` has: a match that runs past it raises `Match_timeout`, and one whose backtracking
memory reaches its cap `Match_memory`. No pattern can overflow the stack. `set_default_match_timeout` sets the timeout
of every pattern compiled after it.

```ocaml
module R = Corvus_regex_ecma262

let () =
  let slow = R.compile_exn ~match_timeout:0.1 {|^(?:(a+)\1*)+$|} in
  assert (match R.is_match slow (String.make 60 'a' ^ "b") with _ -> false | exception R.Match_timeout -> true)
```

## The edition and the version of Unicode

JSON Schema 2020-12 names the grammar of ECMAScript 2020. `~edition:Es2020` refuses what later editions added: the `d`
and `v` flags, modifier groups, a group name used twice, and property names outside the 2020 tables. The version of
Unicode is chosen apart from it with `~unicode_version`: `V13`, `V17`, `V18`, `Latest` (18) or `Default` (V8's, 17).
Both are read when the pattern is compiled, so a match costs the same whatever they are.

```ocaml
module R = Corvus_regex_ecma262

let () =
  let greek =
    R.compile_exn ~flags:"u" ~match_timeout:1.0 ~edition:R.Es2020 ~unicode_version:R.V13 {|^\p{Script=Greek}+$|}
  in
  assert (R.is_match greek "αβγ");
  (* Vithkuqi is in Unicode 14 and later, so not in Unicode 13. *)
  assert (not (R.is_valid ~flags:"u" ~unicode_version:R.V13 {|\p{Script=Vithkuqi}|}))
```

## Differences from Str and Re

- The grammar is ECMA-262's: neither `Str`'s (`\(` for a group) nor `Re`'s choice of Perl, POSIX or Emacs syntax.
  Backreferences, lookahead and lookbehind are there, which `Re` does not have. The flags are a string as `RegExp`
  takes them, and `(?i:...)` groups set them inside a pattern.
- `\d` is `[0-9]`, `\w` the 63 ASCII word characters, `\s` ECMAScript's white space and line terminators, and `.`
  excludes LF, CR, U+2028 and U+2029.
- A replacement template writes a group as `$1` or `$<name>`, and `$&`, `` $` `` and `$'` are the match and the text
  before and after it.
- `split` puts what the groups captured between the pieces, as JavaScript's does.
- A compiled pattern holds no state, so it is shared between threads and domains, which `Str` is not.
- `Re` is linear in the text for every pattern, having no backreferences. Here a pattern without a backreference is
  linear too, and a pattern from outside should be given a `match_timeout`, which bounds every match.

## Where next

- [The API reference](https://ocaml.org/p/corvus-regex-ecma262/latest/doc/index.html) on ocaml.org.
- [The README](../README.md).
- [The benchmark page](https://corvus-oss.org/corvus-regex-ecma262-benchmarks/), with every engine measured nightly.
