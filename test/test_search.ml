(* The tests of the searches for where a match starts, and of the automata they use, ported from the Rust crate's
   tests.rs (by way of the Pascal engine's TestSearch): each search finds what trying every position finds. A pattern is
   compiled here as the library compiles it, and again with Search.no_search set, which makes the search try every
   position its first set allows. The program prints each failure and a final count, and exits with a status other than
   zero when anything failed. *)

module R = Corvus_regex_ecma262
module Parse = Corvus_regex_ecma262__Parse
module Prog = Corvus_regex_ecma262__Prog
module Search = Corvus_regex_ecma262__Search
module Dfa = Corvus_regex_ecma262__Dfa
module Units = Corvus_regex_ecma262__Units
module Vm_state = Corvus_regex_ecma262__Vm_state
module Vm8 = Corvus_regex_ecma262__Vm8
module Vm16 = Corvus_regex_ecma262__Vm16
module Kernels = Corvus_regex_ecma262__Kernels

let failures = ref 0
let checks = ref 0

let check ok what =
  incr checks;
  if not ok then begin
    incr failures;
    if !failures <= 30 then print_endline ("FAIL: " ^ what)
  end

let seed = ref 0x2545F4914F6CDD1D

(* xorshift, over OCaml's 63-bit ints. *)
let next () =
  let s = !seed in
  let s = s lxor (s lsl 13) in
  let s = s lxor (s lsr 7) in
  let s = s lxor (s lsl 17) in
  seed := s;
  s land max_int

let syntax_of_flags flags =
  let has c = String.contains flags c in
  {
    Parse.unicode = has 'u' || has 'v';
    sets = has 'v';
    icase = has 'i';
    multiline = has 'm';
    dot_all = has 's';
  }

(* A pattern's programs, as the library compiles them: the one that records only the groups backreferences name, and
   the one that records every group. *)
type search = { prog : Prog.t; caps : Prog.t; groups : int; unicode : bool; sticky : bool }

(* The search of a pattern, with the search layer or (plain) without; None for a pattern that is not valid. *)
let compile pattern flags plain =
  let sx = syntax_of_flags flags in
  Search.no_search := plain;
  let r =
    match Parse.parse_with pattern sx with
    | root, ps ->
        let count = Parse.group_count ps in
        let prog = Prog.make root sx.unicode None in
        let root, _ = Parse.parse_with pattern sx in
        let caps = Prog.make root sx.unicode (Some count) in
        Some { prog; caps; groups = count; unicode = sx.unicode; sticky = String.contains flags 'y' }
    | exception _ -> None
  in
  Search.no_search := false;
  r

let st = Vm_state.create ()

(* Each match of a global search, with its groups with submatch and its bounds alone otherwise. *)
let all_spans utf8 s (t : Units.text) submatch =
  let out = ref [] and at = ref 0 and fin = ref false in
  while not !fin do
    let prog = if submatch then s.caps else s.prog in
    let from = (if utf8 then Vm8.match_with else Vm16.match_with) prog st t !at s.sticky 0 in
    if from < 0 then fin := true
    else begin
      let to_ = st.end_ in
      out := (if submatch then Array.append [| from; to_ |] (Array.copy st.caps) else [| from; to_ |]) :: !out;
      at :=
        if to_ = from then (if utf8 then Units.Units8.advance else Units.Units16.advance) t to_ s.unicode else to_;
      if !at > t.len then fin := true
    end
  done;
  List.rev !out

let text8 s = { Units.s; base = 0; len = String.length s }

(* UTF-16LE of UTF-8 text. *)
let utf16_of s =
  let b = Buffer.create (2 * String.length s) and k = ref 0 in
  while !k < String.length s do
    let d = String.get_utf_8_uchar s !k in
    Buffer.add_utf_16le_uchar b (Uchar.utf_decode_uchar d);
    k := !k + Uchar.utf_decode_length d
  done;
  Buffer.contents b

let text16 s = { Units.s; base = 0; len = String.length s / 2 }

let compare_on searched plain pattern flags text =
  List.iter
    (fun submatch ->
      check
        (all_spans true plain (text8 text) submatch = all_spans true searched (text8 text) submatch)
        (Printf.sprintf "/%s/%s over UTF-8 %S differs from every position" pattern flags text);
      let t16 = text16 (utf16_of text) in
      check
        (all_spans false plain t16 submatch = all_spans false searched t16 submatch)
        (Printf.sprintf "/%s/%s over UTF-16 %S differs from every position" pattern flags text))
    [ true; false ]

let rec pairs = function a :: b :: rest -> (a, b) :: pairs rest | _ -> []
let rec triples = function a :: b :: c :: rest -> (a, b, c) :: triples rest | _ -> []

let test_searches_find_what_every_position_finds () =
  let patterns =
    [ "Sherlock"; ""; "Sherlock|Street"; ""; "Sherlock|Holmes|Watson|Irene|Adler|John|Baker"; "i";
      "Sher[a-z]+|Hol[a-z]+"; "i"; "[a-q][^u-z]{13}x"; ""; "[a-zA-Z]+ing"; ""; "\\s[a-zA-Z]{0,12}ing\\s"; "";
      "\\w+\\s+Holmes"; ""; "\\w+\\s+Holmes\\s+\\w+"; ""; "\\b\\w+n\\b"; ""; "\\w+n\\b"; ""; ".*$"; ""; "(?:.)??$"; "";
      ".*\\s"; ""; "abcd|bc"; ""; "(?:x|y)*z|q"; ""; "\\b(?:the|he)\\b"; "i"; "Holmes.{0,25}Watson|Watson.{0,25}Holmes";
      ""; "Tom.{1,5}er|er.{1,5}Tom"; ""; "\\w+n(?=\\s)"; ""; "[a-z]+n\\B"; "i"; "\\w+\\s+Holmes$"; "m";
      "[a-z]+ing\\b|x"; "";
      ".{0,2}(Tom|Sawyer|Huckleberry|Finn)"; ""; ".{2,4}(Tom|Sawyer|Huckleberry|Finn)"; "";
      "Tom.{10,25}river|river.{10,25}Tom"; ""; "([A-Za-z]awyer|[A-Za-z]inn)\\s"; "";
      "[\\w\\.+-]+@[\\w\\.-]+\\.[\\w\\.-]+"; "";
      "\\p{Sm}"; ""; "(?:(?:25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9])\\.){3}(?:25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9])"; "";
      "Шерлок Холмс|Джон Уотсон"; "i"; "夏洛克|福尔摩斯"; ""; "sk"; "i"; "ks"; "i"; "\\u{1F600}a|b\\u{10400}"; "";
      "a\\u{1F600}"; ""; "\\ud83d"; ""; "x*y"; ""; "[a-c]*b+c"; ""; "(?:ab)?c"; ""; "\\béé\\w*"; "";
      (* One literal with no ASCII byte rare enough alone: a capital's last byte alone, then the two rarest bytes. *)
      "Шерлок"; ""; "Éa"; ""; "aé"; ""; "洛克"; "";
      "(?<=a)b|c"; ""; "[\\p{L}--[a-z]]+t"; "v"; "[\\q{abc|de}x]y"; "v"; "\\w+"; ""; ".*"; ""; ".+"; "s"; "[^\\n]*"; "";
      "[^\\n.]+"; ""; "\\p{L}"; ""; "[\\ud800-\\udfff]"; ""; "Tom|river|Tomas"; ""; "ab|a"; "";
      "sherlock|holmes|k"; "i";
      "\\u{1F600}|Ш"; ""; "(|([^a]|[^a][^a]){2,4}){2}(\\3){3,5}?x"; ""; "[^a]{0,3}(?:xx|x)"; "";
      "\\s[a-zA-Z]{0,12}ing\\s"; ""; "\\d{3}-\\d{4}"; ""; "[0-9]+\\.[0-9]+"; ""; "(?:\\d+\\.){3}\\d+"; "";
      "\\b\\d\\d:\\d\\d\\b"; ""; "[A-Z]{2}\\d{2}"; ""; "(?:1|22|333)\\.5\\.\\d"; ""; "\\d{2}[é5]\\d"; "u";
      "\\d{3}[éèêë5]"; "u";
      "^abc"; "m"; "x{0}^abc"; ""; "\\babc\\b"; ""; "abc\\s+\\w+"; ""; "Sherlock\\s+\\w+"; ""; "a|bc|def"; "";
      "colou?r|grey"; "i"; "[^a]+b"; ""; "(?:a|b)+c"; ""; "\\d+"; ""; "[\\d,]+"; ""; "x(?:y|z)w"; ""; "ab*c|d"; "" ]
  [@@ocamlformat "disable"]
  in
  let alphabet =
    [| "a"; "b"; "c"; "e"; "i"; "k"; "n"; "s"; "t"; "x"; "y"; "g"; "S"; "K"; " "; "\n"; "."; "@"; "1"; "2"; "5"; "+";
       "="; "ſ"; "K"; "é"; "Ш"; "е"; "р"; "л"; "о"; "к"; "Х"; "夏"; "洛"; "克"; "😀"; "𐐀"; "ing"; "Tom"; "river";
       "Holmes"; "Sherlock"; "sherLOCK"; "awyer"; "inn"; "Watson"; "Шерлок Холмс"; "夏洛克"; "abc"; "de"; "0"; "25"; "É";
       "255."; "192.168."; "333.5."; "12:34"; "555-1234"; "AB12"; "123é"; "def" |]
  [@@ocamlformat "disable"]
  in
  seed := 0x2545F4914F6CDD1D;
  let texts =
    Array.init 60 (fun _ ->
        let n = 1 + (next () mod 300) in
        String.concat "" (List.init n (fun _ -> alphabet.(next () mod Array.length alphabet))))
  in
  List.iter
    (fun (pattern, base) ->
      List.iter
        (fun mode ->
          let flags = if base = "v" then "v" else base ^ mode in
          match compile pattern flags false with
          | None -> ()
          | Some searched -> (
              match compile pattern flags true with
              | None -> check false ("/" ^ pattern ^ "/" ^ flags ^ " compiles without the search")
              | Some plain ->
                  check
                    (match plain.prog.search.plan with Search.Scan -> true | _ -> false)
                    (Printf.sprintf "/%s/%s without the search scans" pattern flags);
                  Array.iter (compare_on searched plain pattern flags) texts))
        [ ""; "u" ])
    (pairs patterns)

let test_starts_the_fuzzer_found () =
  let cases =
    [ "\\b\\w+n\\b"; ""; "a nation"; "\\b\\w+n\\b"; ""; "an nation nn nan noon"; "\\w+\\s+Holmes"; ""; "Holmes Holmes";
      "\\b\\d*\\s*a\\b"; ""; " a"; "\\b\\d*\\W*x\\b"; "s"; " ééx"; "\\b\\d*\\W*x"; ""; "a1 x"; "\\b\\d*\\W+\\w+ab"; "";
      "ab0 aab";
      "\\b\\w*[\\u{1F600}-\\u{1F64F}]*S\\B"; "u"; "😀S_n😀S"; "\\W+[a-z]*\\W*a"; "m"; " a"; "[a-zéè]+\\d*[a-z]+on"; "i";
      " ONaON"; "\\w+?[^\\w\\s]*\\d+?n"; "im"; "000n"; "\\w*\\W+-$"; ""; " a\xC2\xA0-a--"; "\\w*[^\\w\\s]+k$"; "";
      "---_-ka.k"; "\\b[0-9a-f]*ét"; ""; "\nét0aét"; ".{1,3}(?:on|\\.)."; ""; "😀😀😀ona on";
      "\\b[^\\n]{1,3}(?:k|\\.)\\b"; "im"; "k 😀nk"; "[^\\n]{1,3}(?:é|a)"; ""; "中中中a"; "\\B[^\\w\\s]{1,3}(?:a|n)"; "";
      "a😀a";
      (* A later occurrence more than hi units on, and within hi characters, allows an earlier start than the one an
         occurrence's window holds. *)
      "(|([^a]|[^a][^a]){2,4}){2}(\\3){3,5}?x"; ""; "bxſſſſſſſſx" ]
  [@@ocamlformat "disable"]
  in
  List.iter
    (fun (pattern, base, text) ->
      List.iter
        (fun mode ->
          let flags = base ^ mode in
          match (compile pattern flags false, compile pattern flags true) with
          | Some searched, Some plain -> compare_on searched plain pattern flags text
          | _ -> ())
        [ ""; "u" ])
    (triples cases)

(* The starts of the occurrences of a literal, by a plain search of the string. *)
let occurrences lit text =
  let n = String.length lit and out = ref [] and i = ref 0 in
  while !i + n <= String.length text do
    if String.sub text !i n = lit then begin
      out := !i :: !out;
      i := !i + n
    end
    else incr i
  done;
  List.rev !out

let test_rare_literal () =
  (* Twain, Tw and aIa are searched for with a second byte (a T or an I is common), over texts longer than a block. *)
  let literals = [ "zqj"; "aqj"; "qq"; "Xa"; "abZ"; "jj j"; "Twain"; "Tw"; "aIa" ] in
  let letters = "abqjzZX TwinI" in
  seed := 0x2F6B1C3D;
  List.iter
    (fun lit ->
      let re = R.compile_exn lit in
      for _ = 1 to 200 do
        let n = next () mod 400 in
        let text = String.init n (fun _ -> letters.[next () mod String.length letters]) in
        let want = occurrences lit text in
        let got8 = List.of_seq (Seq.map R.Match.start (R.matches re text)) in
        let got16 = List.of_seq (Seq.map R.Match.start (R.matches_utf16 re (utf16_of text))) in
        check (got8 = want && got16 = want) (Printf.sprintf "the literal %s in %S" lit text)
      done)
    literals;
  (* Twain at every offset of the first blocks of its search: each lane of a block finds it. *)
  let re = R.compile_exn "Twain" in
  for k = 0 to 140 do
    let text = String.make k 'a' ^ "Twain" ^ String.make (200 - k) 'T' in
    check (match R.find re text with Some m -> R.Match.start m = k | None -> false) (Printf.sprintf "Twain at %d" k)
  done;
  (* Literals of one byte to three at the very end of the text, for the table finder's last starts. *)
  List.iter
    (fun lit ->
      let re = R.compile_exn lit in
      for k = 0 to 40 do
        let tail = String.sub lit (String.length lit - 1) 1 in
        let text = String.make k 'x' ^ tail in
        let want = occurrences "b" text <> [] || occurrences "c" text <> [] in
        check (R.is_match re text = want) (Printf.sprintf "/%s/ at the end of %S" lit text)
      done)
    [ "a|b"; "ab|b"; "a|ac|c" ]

let test_dot_all_run () =
  let texts =
    [| []; [ 0x61; 0xD800 ]; [ 0x61; 0xDC00 ]; [ 0xDC00; 0x61; 0xD83D; 0xDE00 ]; [ 0xD83D; 0xDE00 ];
       [ 0xDC00; 0x62; 0xD83D; 0xDE00; 0xD800; 0x63; 0xDBFF ] |]
  in
  let u16 units =
    let b = Bytes.create (2 * List.length units) in
    List.iteri (fun k u -> Bytes.set_uint16_le b (2 * k) u) units;
    Bytes.to_string b
  in
  (* A start between the halves of a pair is moved to the pair's start, as the library does before it searches. *)
  let norm s t at = Units.Units16.normalize t at s.unicode in
  let span s t at =
    let from = Vm16.match_with s.caps st t (norm s t at) s.sticky 0 in
    if from >= 0 then Some (from, st.end_) else None
  in
  let mid_pair (units : int array) at =
    at > 0 && at < Array.length units && units.(at) >= 0xDC00 && units.(at) <= 0xDFFF && units.(at - 1) >= 0xD800
    && units.(at - 1) <= 0xDBFF
  in
  List.iter
    (fun flags ->
      let unicode = String.contains flags 'u' || String.contains flags 'v' in
      let get p = match compile p flags false with Some s -> s | None -> failwith p in
      let shortcut = get ".*" and matcher = get "(.*)" and plus = get ".+" and plus_matcher = get "(.+)" in
      check
        (match shortcut.prog.search.shape with Search.Loop _ -> true | _ -> false)
        (".* is the loop shape with " ^ flags);
      Array.iteri
        (fun k units ->
          let a = Array.of_list units in
          let t = text16 (u16 units) in
          for start = 0 to Array.length a do
            let from = if unicode && mid_pair a start then start - 1 else start in
            let len = Array.length a in
            let shape_span =
              let f = Vm16.match_with shortcut.prog st t (norm shortcut t start) shortcut.sticky 0 in
              if f >= 0 then Some (f, st.end_) else None
            in
            check
              (shape_span = span matcher t start && shape_span = Some (from, len))
              (Printf.sprintf ".* with %s from %d of text %d" flags start k);
            let plus_span =
              let f = Vm16.match_with plus.prog st t (norm plus t start) plus.sticky 0 in
              if f >= 0 then Some (f, st.end_) else None
            in
            if from < len then
              check
                (plus_span = span plus_matcher t start && plus_span = Some (from, len))
                (Printf.sprintf ".+ with %s from %d of text %d" flags start k)
            else
              check
                (plus_span = None && span plus_matcher t start = None)
                (Printf.sprintf ".+ with %s from %d of text %d finds nothing" flags start k)
          done)
        texts)
    [ "s"; "su"; "sv"; "sy"; "suy" ]

let test_forward_kept () =
  (match
     compile "(?:(?:25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9])\\.){3}(?:25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9])" "" false
   with
  | Some s ->
      check
        (match s.prog.forward with Some f -> Corvus_regex_ecma262__Once.get f <> None | None -> false)
        "the forward automaton is built for the scan";
      check (s.prog.skip <> None) "the skip window is found"
  | None -> check false "the address pattern compiles");
  match compile "\\d{3}-\\d{4}" "" true with
  | Some s -> check (s.prog.forward = None) "no forward automaton without the search"
  | None -> check false "the telephone pattern compiles"

let test_rare_unit16 () =
  let cases =
    [ "Twain"; "u"; "Twi Twa Twain Twain Twai"; "aqj"; "u"; "q aq aqj qj aqjaqj"; "zqj"; ""; "zq zqj"; "Шерлок"; "u";
      "Ш Шерлок шерлок"; "夏洛克"; "u"; "夏洛 夏洛克夏洛克"; "a😀b"; "u"; "a😀 a😀b"; "Sherlock|Street"; "u";
      "Sher Street Sherlock St"; "Sherlock|Street"; "y"; "Street" ]
  [@@ocamlformat "disable"]
  in
  List.iter
    (fun (pattern, flags, text) ->
      match (compile pattern flags false, compile pattern flags true) with
      | Some searched, Some plain ->
          let t = text16 (utf16_of text) in
          check (all_spans false plain t false = all_spans false searched t false) ("the UTF-16 search for " ^ pattern);
          for from = 0 to t.len do
            let from = Units.Units16.normalize t from searched.unicode in
            let f1 = Vm16.match_with searched.prog st t from searched.sticky 0 in
            let e1 = st.end_ in
            let f2 = Vm16.match_with plain.prog st t from plain.sticky 0 in
            let e2 = st.end_ in
            check (f1 = f2 && (f1 < 0 || e1 = e2)) (Printf.sprintf "the UTF-16 search for %s from %d" pattern from)
          done
      | _ -> check false (pattern ^ " compiles"))
    (triples cases)

let test_plans () =
  let plan_name (s : Search.t) =
    match s.plan with Search.Scan -> "scan" | Windows _ -> "windows" | After_loops _ -> "after-loops"
  in
  let shape_name (s : Search.t) =
    match s.shape with
    | Search.No_shape -> "none"
    | Literals_only -> "literals"
    | Forms _ -> "forms"
    | Set_shape _ -> "set"
    | Loop _ -> "loop"
  in
  let reverse (s : Search.t) = match s.plan with Windows { reverse = Some _; _ } -> true | _ -> false in
  List.iter
    (fun (pattern, flags, plan, shape, rev) ->
      match compile pattern flags false with
      | Some s ->
          check
            (plan_name s.prog.search = plan && shape_name s.prog.search = shape && reverse s.prog.search = rev)
            ("the plan of " ^ pattern)
      | None -> check false (pattern ^ " compiles"))
    [ ("Sherlock", "", "windows", "literals", false);
      ("Sherlock|Holmes|Watson", "i", "windows", "forms", false);
      ("[a-zA-Z]+ing", "", "after-loops", "none", false);
      (".{0,2}(Tom|Sawyer|Huckleberry|Finn)", "", "windows", "none", true);
      ("\\w+", "", "scan", "loop", false);
      ("\\p{Sm}", "u", "scan", "set", false) ];
  (match compile "Sherlock\\s+\\w+" "" false with
  | Some s ->
      check (s.prog.after_literal <> None && s.prog.anchored_dfa <> None) "the automaton goes on after the literal"
  | None -> check false "Sherlock\\s+\\w+ compiles");
  match compile "\\b\\w+n\\b" "" false with
  | Some s -> check s.prog.search.leading_word_boundary "a leading word boundary is passed over inside a word"
  | None -> check false "\\b\\w+n\\b compiles"

(* The automaton over ASCII agrees with the matcher over ASCII texts, for patterns it takes, and decides every ASCII
   text. *)
let test_automaton () =
  let patterns =
    [ "^[a-z]+\\d*$"; ""; "\\bfoo\\b"; ""; "\\Bo+\\B"; "u"; "(a|ab)(c|bcd)(d*)"; ""; "colou?r"; "i"; "^$"; "";
      "x{2,4}y"; "";
      "[^\\s]+@[^\\s]+"; "u"; "a|"; ""; "(?:ab)*c"; "" ]
  [@@ocamlformat "disable"]
  in
  let texts =
    [ ""; "a"; "abc123"; "foo"; "a foo b"; "food"; "xxy"; "xxxxxy"; "COLOR"; "colour"; "a@b"; "abcd"; "ababc"; "éfoo";
      " oo " ]
  in
  List.iter
    (fun (pattern, flags) ->
      let root, ps = Parse.parse_with pattern (syntax_of_flags flags) in
      check
        ((not ps.Parse.needs_backtracking) && Dfa.re2_cost root <= Dfa.max_re2_cost)
        ("the automaton takes " ^ pattern);
      match Dfa.build root with
      | None -> check false ("an automaton for " ^ pattern)
      | Some d ->
          (* find asks the matcher, and never the automaton. *)
          let re = R.compile_exn ~flags pattern in
          List.iter
            (fun text ->
              let want = R.find re text <> None in
              let ascii = String.for_all (fun c -> Char.code c < 0x80) text in
              (match Vm8.dfa_is_match d (text8 text) with
              | -1 ->
                  (* A text beyond ASCII may still be decided before its first such unit (a|, which accepts at once). *)
                  check (not ascii) (Printf.sprintf "the automaton of %s decides %S" pattern text)
              | got -> check (got = 1 = want) (Printf.sprintf "the automaton of %s over %S" pattern text));
              match Vm16.dfa_is_match d (text16 (utf16_of text)) with
              | -1 -> ()
              | got -> check (got = 1 = want) (Printf.sprintf "the automaton of %s over UTF-16 %S" pattern text))
            texts)
    (pairs patterns)

(* The searches again with the processor's vector kernels turned off: the byte shuffle of Teddy and the 32-byte
   loads. *)
let test_without_vectors () =
  let shuffle = !Kernels.has_shuffle and wide = !Kernels.has_wide in
  Kernels.has_shuffle := false;
  Kernels.has_wide := false;
  test_rare_literal ();
  test_rare_unit16 ();
  Kernels.has_shuffle := shuffle;
  Kernels.has_wide := wide

(* The skip window's run search (Kernels.index_run) finds what a plain search for the first run finds: random texts of a
   few byte values, random ranges, every length of run from 1 to 64 and every start. *)
let test_index_run () =
  seed := 0x5DEECE66D;
  let reference s from n (ranges : string) nr len =
    let in_ranges c =
      let hit = ref false in
      for q = 0 to nr - 1 do
        if (c - Char.code ranges.[2 * q]) land 0xFF <= Char.code ranges.[(2 * q) + 1] then hit := true
      done;
      !hit
    in
    let run = ref 0 and found = ref (-1) and k = ref from in
    while !found < 0 && !k < n do
      if in_ranges (Char.code s.[!k]) then begin
        incr run;
        if !run >= len then found := !k - len + 1
      end
      else run := 0;
      incr k
    done;
    !found
  in
  for _ = 1 to 300 do
    let n = next () mod 400 in
    let alphabet = "ab1-9 \x80\xff" in
    let s = String.init n (fun _ -> alphabet.[next () mod String.length alphabet]) in
    let nr = 1 + (next () mod 4) in
    let ranges = String.init (2 * nr) (fun k -> Char.chr (if k mod 2 = 0 then next () mod 256 else next () mod 64)) in
    let len = 1 + (next () mod 64) in
    let from = if n = 0 then 0 else next () mod n in
    check
      (Kernels.index_run s from n ranges nr len = reference s from n ranges nr len)
      (Printf.sprintf "index_run of a run of %d from %d over %d bytes" len from n)
  done

(* The end of a run of a set's ASCII members (Kernels.ascii_run_end) is where a plain loop stops: random sets, texts that
   hold bytes beyond ASCII, every start. *)
let test_ascii_run_end () =
  seed := 0x1234567;
  for _ = 1 to 300 do
    let members = Array.init 128 (fun _ -> next () mod 3 <> 0) in
    let cs =
      Corvus_regex_ecma262__Charset.make
        (Array.concat (List.filter_map (fun c -> if members.(c) then Some [| c; c |] else None) (List.init 128 Fun.id)))
    in
    let n = next () mod 200 in
    let s = String.init n (fun _ -> Char.chr (if next () mod 20 = 0 then 0x80 + (next () mod 128) else next () mod 128)) in
    let from = if n = 0 then 0 else next () mod n in
    let k = ref from in
    while !k < n && Char.code s.[!k] < 0x80 && members.(Char.code s.[!k]) do
      incr k
    done;
    check
      (Kernels.ascii_run_end s from n cs.nibbles = !k)
      (Printf.sprintf "ascii_run_end from %d over %d bytes" from n)
  done

(* The search for ranges of UTF-16 units (Kernels.index_units16) finds what a plain loop finds: random texts and up to
   eight random ranges, every start. *)
let test_index_units16 () =
  seed := 0x9E3779B9;
  for _ = 1 to 300 do
    let n = next () mod 120 in
    let units = Array.init n (fun _ -> if next () mod 4 = 0 then next () mod 0x10000 else 0x40 + (next () mod 64)) in
    let b = Bytes.create (2 * n) in
    Array.iteri (fun k u -> Bytes.set_uint16_le b (2 * k) u) units;
    let nr = 1 + (next () mod 8) in
    let pairs = Array.init nr (fun _ -> let lo = next () mod 0x10000 in (lo, min 0xFFFF (lo + (next () mod 40)))) in
    let r = Bytes.create (4 * nr) in
    Array.iteri
      (fun q (lo, hi) ->
        Bytes.set_uint16_le r (4 * q) lo;
        Bytes.set_uint16_le r ((4 * q) + 2) (hi - lo))
      pairs;
    let from = if n = 0 then 0 else next () mod n in
    let k = ref from and found = ref (-1) in
    while !found < 0 && !k < n do
      if Array.exists (fun (lo, hi) -> units.(!k) >= lo && units.(!k) <= hi) pairs then found := !k else incr k
    done;
    check
      (Kernels.index_units16 (Bytes.to_string b) 0 from n (Bytes.to_string r) nr = !found)
      (Printf.sprintf "index_units16 from %d over %d units" from n)
  done

(* The UTF-16 form of the ASCII run search (Kernels.ascii_run_end16): random sets, texts with units beyond ASCII (and
   beyond 0x7FFF), every start. *)
let test_ascii_run_end16 () =
  seed := 0x2545F491;
  for _ = 1 to 300 do
    let members = Array.init 128 (fun _ -> next () mod 3 <> 0) in
    let cs =
      Corvus_regex_ecma262__Charset.make
        (Array.concat (List.filter_map (fun c -> if members.(c) then Some [| c; c |] else None) (List.init 128 Fun.id)))
    in
    let n = next () mod 120 in
    let units =
      Array.init n (fun _ ->
          match next () mod 20 with 0 -> 0x80 + (next () mod 0x7F80) | 1 -> 0x8000 + (next () mod 0x8000) | _ -> next () mod 128)
    in
    let b = Bytes.create (2 * n) in
    Array.iteri (fun k u -> Bytes.set_uint16_le b (2 * k) u) units;
    let from = if n = 0 then 0 else next () mod n in
    let k = ref from in
    while !k < n && units.(!k) < 0x80 && members.(units.(!k)) do
      incr k
    done;
    check
      (Kernels.ascii_run_end16 (Bytes.to_string b) 0 from n cs.nibbles = !k)
      (Printf.sprintf "ascii_run_end16 from %d over %d units" from n)
  done

let () =
  test_ascii_run_end16 ();
  test_index_units16 ();
  test_ascii_run_end ();
  test_index_run ();
  test_searches_find_what_every_position_finds ();
  test_starts_the_fuzzer_found ();
  test_rare_literal ();
  test_dot_all_run ();
  test_forward_kept ();
  test_rare_unit16 ();
  test_plans ();
  test_automaton ();
  test_without_vectors ();
  Printf.printf "%d checks, %d failed\n" !checks !failures;
  if !failures > 0 then exit 1
