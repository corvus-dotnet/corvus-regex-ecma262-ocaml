# Getting started with OCaml

## Introduction

`corvus-regex-ecma262` is an OCaml library whose patterns answer as ECMAScript does. Where `Str` and `Re` have their
own grammars and their own meaning for `\d`, `\w`, `\s`, `.` and case, this library has ECMA-262's, so a pattern
written for JavaScript, or taken from a JSON Schema, matches the same texts here as it does there. It is a port of the
corvus-regex-ecma262 Rust crate, held to V8's answers on the ECMA-262 test data, with no dependencies beyond the
compiler's distribution.

This guide covers:

- **Setup**: installing the library with opam.
- **Patterns and flags**: the flags are ECMA-262's letters, and what each one does.
- **The edition and the version of Unicode**: the grammar of ECMAScript 2020, and the Unicode tables a pattern reads.
- **Matching**: `is_match`, `find`, `matches`, `count`, `replace` and `split`, and the forms that allocate nothing.
- **Timeouts**: the limit of time and of memory a match may take.
- **UTF-8 and UTF-16**: strings as UTF-8, checked once with `utf8_text`, and strings of UTF-16LE code units.
- **Differences from Str and Re**: what to expect when they meet.

## Setup

```
opam install corvus-regex-ecma262
```

It needs OCaml 4.14 or later, 64-bit, on Linux, macOS or Windows. In a dune project, add the library to a stanza's
`libraries`:

```
(executable
 (name main)
 (libraries corvus-regex-ecma262))
```

The module is `Corvus_regex_ecma262`; the examples below name it `R`.

## Patterns and flags

A pattern is written as it would be in JavaScript, and the flags are the letters `RegExp` takes, as the string
`?flags`:

| Flag | Meaning |
|---|---|
| `d` | Accepted for completeness; indices are always kept. |
| `g` | `replace` replaces every match; without it, the first, as `String.prototype.replace` does. |
| `i` | Canonicalize of the mode in force: simple case folding with `u` or `v`, `toUpperCase` without. Never the locale. |
| `m` | `^` and `$` at line terminators. |
| `s` | The dot matches line terminators. |
| `u` | The `u` flag grammar; matched by code point. |
| `v` | Set notation and properties of strings; matched by code point. `u` and `v` together is an error, as in ECMA-262. |
| `y` | A match only at the start position. |

```ocaml
module R = Corvus_regex_ecma262

let () =
  let a = R.compile_exn ~flags:"u" {|\p{Script=Greek}+|} in
  let b = R.compile_exn ~flags:"v" {|[[a-z]--[aeiou]]+|} in
  let c = R.compile_exn ~flags:"u" {|^(?<year>\d{4})-(?<month>\d{2})$|} in
  assert (R.is_match a "αβγ");
  assert (R.is_match b "rhythm");
  assert (R.group_names c = [ "year"; "month" ])
```

With no flag the pattern is read with the Annex B grammar and matched by UTF-16 code unit, which is what ECMA-262 does
with no flag. JSON Schema's reading, the `u` flag first and Annex B when that refuses, is two calls of `compile`, which
returns a `result` and takes the match timeout, so it costs no exception and needs no default timeout:

```ocaml
module R = Corvus_regex_ecma262

let schema_pattern pattern =
  match R.compile ~flags:"u" ~match_timeout:1.0 pattern with
  | Ok re -> Ok re
  | Error _ -> R.compile ~match_timeout:1.0 pattern

let () =
  assert (match schema_pattern {|^\p{L}+$|} with Ok re -> R.flags_string re = "u" | Error _ -> false);
  (* An identity escape, which only Annex B allows. *)
  assert (match schema_pattern {|\-|} with Ok re -> R.flags_string re = "" | Error _ -> false)
```

A pattern that does not compile gives an `error`: `Invalid` with the reason, `Flags`, `Too_large` (with the limit it is
over: `max_pattern_length`, `max_nesting` or `max_size`) or `Invalid_utf8`. `compile_exn` raises it as
`Compile_error`, and `error_message` says what it is.

```ocaml
module R = Corvus_regex_ecma262

let () =
  assert (match R.compile ~flags:"u" "a{2,1}" with Error (R.Invalid _) -> true | _ -> false);
  assert (match R.compile ~flags:"uv" "a" with Error (R.Flags _) -> true | _ -> false);
  assert (match R.compile_exn "(" with _ -> false | exception R.Compile_error (R.Invalid _) -> true)
```

## The edition and the version of Unicode

By default a pattern is read with today's grammar, ECMAScript 2025 (`Current`), and with the tables of the version of
Unicode that V8 carries, 17 today (`Default`). Either can be overridden when the pattern is compiled, each independently
of the other.

JSON Schema 2020-12 names the grammar of ECMAScript 2020, which `~edition:Es2020` selects in place of the default. It
refuses what later editions added, and nothing else: the `d` and `v` flags, modifier groups such as `(?i:...)`, a group
name used by two groups, and a property name or value that is not in the 2020 tables (such as `\p{Script=Yezidi}`). A
pattern it accepts has the same answers as in today's grammar.

The version of Unicode is chosen apart from the edition: `~unicode_version` is `V13`, `V17`, `V18`, `Latest` (18 today)
or `Default`, the version of the V8 the answers are held to (17 today). It decides the members of each `\p{...}`, the
characters of a group name and the case folding of the `i` flag. `unicode_version_of` reports the version a pattern
was compiled with. Both settings are read when the pattern is compiled, so a match costs the same whichever is chosen.

```ocaml
module R = Corvus_regex_ecma262

let () =
  (* ECMAScript 2020 as its edition was published, with the Unicode of its year. *)
  let greek =
    R.compile_exn ~flags:"u" ~match_timeout:1.0 ~edition:R.Es2020 ~unicode_version:R.V13 {|^\p{Script=Greek}+$|}
  in
  assert (R.is_match greek "αβγ");
  assert (R.unicode_version_of greek = R.V13);
  (* Vithkuqi is in Unicode 14 and later, so not in Unicode 13. *)
  assert (not (R.is_valid ~flags:"u" ~unicode_version:R.V13 {|\p{Script=Vithkuqi}|}))
```

## Matching

The functions are those of a `RegExp`, with the position as `?pos` where JavaScript keeps `lastIndex`:

```ocaml
module R = Corvus_regex_ecma262

let () =
  let date = R.compile_exn ~flags:"gu" {|(?<year>\d{4})-(?<month>\d{2})|} in
  let text = "due 2026-10, then 2027-01" in
  assert (R.is_match date text);
  (match R.find date text with
   | Some m ->
       assert (R.Match.value m = "2026-10" && R.Match.start m = 4 && R.Match.end_ m = 11);
       assert (R.Match.named m "year" = Some "2026" && R.Match.group m 2 = Some "10")
   | None -> assert false);
  (* Every match of a global search, as matchAll makes it. *)
  assert (List.map R.Match.value (List.of_seq (R.matches date text)) = [ "2026-10"; "2027-01" ]);
  assert (R.count date text = 2);
  (* replace replaces every match with the g flag, and the first without it, with every $ form of ECMA-262. *)
  assert (R.replace date text "$<month>/$1" = "due 10/2026, then 01/2027");
  (* split puts what the groups captured between the pieces; a group that did not take part is None. *)
  assert (R.split (R.compile_exn {|\s*(;)\s*|}) "a ; b;c" = [ Some "a"; Some ";"; Some "b"; Some ";"; Some "c" ])
```

`find ~pos` is `exec` with `lastIndex` set to `pos`, and `y` anchors it there. `find`, `matches`, `replace` and `split`
allocate what they return. `is_match`, `count` and `exec_into`, which writes a match's groups into a `captures` made
once, allocate nothing once the working memory has grown:

```ocaml
module R = Corvus_regex_ecma262

let () =
  let date = R.compile_exn ~flags:"u" {|(?<year>\d{4})-(?<month>\d{2})|} in
  let groups = R.captures date in
  assert (R.exec_into date groups ~pos:0 "due 2026-10");
  assert (R.capture_start groups 1 = 4 && R.capture_end groups 1 = 8)
```

A compiled pattern holds no mutable state, so it is shared freely between threads (OCaml 4.14) and domains (OCaml 5).

## Timeouts

A match has no limit by default. A pattern compiled with `~match_timeout` (in seconds) abandons a match that reaches
it with `Match_timeout`, and one that reaches the cap on backtracking memory the timeout brings with `Match_memory`,
whatever the pattern:

```ocaml
module R = Corvus_regex_ecma262

let () =
  let slow = R.compile_exn ~match_timeout:0.1 {|^(?:(a+)\1*)+$|} in
  assert (R.match_timeout slow = Some 0.1);
  assert (match R.is_match slow (String.make 60 'a' ^ "b") with _ -> false | exception R.Match_timeout -> true)
```

A pattern compiled without a timeout takes `default_match_timeout ()`, which is none unless
`set_default_match_timeout` sets it. A program that matches patterns or texts from outside should set one, for example
with `set_default_match_timeout (Some 1.0)` when it starts.

A pattern without a backreference is matched in time linear in the text, and no pattern can overflow the stack, so the
timeout is what bounds the others.

## UTF-8 and UTF-16

A string is UTF-8, matched as its bytes with no transcoding, and positions are byte offsets. It is checked before it
is read: a string that is not UTF-8 raises `Invalid_argument`, and is never matched or read as U+FFFD. `utf8_text`
checks a text once, for a text matched many times, and the `_text` functions take it. The `_utf16` functions take a
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
  assert (Option.map span (R.find dot_b "a😀b") = Some (1, 6)); (* bytes of UTF-8 *)
  assert (Option.map span (R.find_utf16 dot_b (utf16 "a😀b")) = Some (1, 4)); (* UTF-16 code units *)
  assert (R.is_match_text dot_b (R.utf8_text_exn "a😀b"));
  assert (match R.is_match dot_b "\xff" with _ -> false | exception Invalid_argument _ -> true)
```

Under the `u` and `v` flags a character is a code point in both encodings, so a class, a dot or a quantifier takes a
four-byte sequence as one character. With no flag ECMA-262 matches by UTF-16 code unit; over UTF-8 the library keeps
that meaning, so a match can start or end between the halves of a character beyond the Basic Multilingual Plane, at
the character's third byte.

## Differences from Str and Re

The functions follow `RegExp`; the meaning follows ECMA-262. The differences a user coming from `Str` or `Re` will meet:

- **The grammar.** Neither `Str`'s (`\(` for a group) nor `Re`'s choice of Perl, POSIX or Emacs syntax. Backreferences,
  lookahead and lookbehind are there, which `Re` does not have. The flags are a separate string, and `(?i:...)` groups
  set them inside a pattern.
- **The character classes.** `\d` is `[0-9]`, `\w` is the 63 ASCII word characters, `\s` is ECMAScript's white space
  and line terminators, and `.` excludes the four line terminators (LF, CR, U+2028, U+2029).
- **Replacement.** A template writes a group as `$1` or `$<name>`, and `$&`, `` $` `` and `$'` are the match and the
  text before and after it; `replace` replaces every match only with the `g` flag, and `replace_all` always does.
- **Split.** `split` is JavaScript's, which puts what the groups captured between the pieces.
- **State.** A compiled pattern holds no state, so it is shared between threads and domains; `Str` keeps the last
  match in global state.
- **Timeouts.** `Re` is linear for every pattern, having no backreferences. Here a pattern without a backreference is
  linear too, and a timeout bounds the others.
