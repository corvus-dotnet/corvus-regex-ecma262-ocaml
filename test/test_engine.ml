(* The tests of the API: flags, the errors of compiling, matches and their groups, the global search, replace, split,
   the refusal of text that is not UTF-8, UTF-16 text, half characters, the match timeout and the linear time of a
   pattern without backreferences. The ECMA-262 test data (tools/dataset/run.ml) is what holds the answers to V8; these
   hold the API to its documentation. The program prints each failure and a final count, and exits with a status other
   than zero when anything failed. *)

module R = Corvus_regex_ecma262

let failures = ref 0
let checks = ref 0

let check ok what =
  incr checks;
  if not ok then begin
    incr failures;
    print_endline ("FAIL: " ^ what)
  end

let compile ?(flags = "") ?match_timeout p = R.compile_exn ~flags ?match_timeout p

(* UTF-16LE text from code units. *)
let u16 units =
  let b = Bytes.create (2 * List.length units) in
  List.iteri (fun k u -> Bytes.set_uint16_le b (2 * k) u) units;
  Bytes.to_string b

let ascii16 s = u16 (List.init (String.length s) (fun k -> Char.code s.[k]))
let raises_invalid f = match f () with _ -> false | exception Invalid_argument _ -> true

let test_flags () =
  check
    (match R.parse_flags "ysvmigd" with Ok f -> R.flags_to_string f = "dgimsvy" | Error _ -> false)
    "flags are written in the order dgimsuvy";
  check (match R.parse_flags "x" with Error (R.Flags _) -> true | _ -> false) "an unknown flag";
  check (match R.parse_flags "gg" with Error (R.Flags _) -> true | _ -> false) "a flag given twice";
  check (match R.parse_flags "uv" with Error (R.Flags _) -> true | _ -> false) "u with v"

let test_compile_errors () =
  check (match R.compile "a{2,1}" with Error (R.Invalid _) -> true | _ -> false) "counts out of order";
  check (match R.compile ~flags:"q" "\\p{Lu}" with Error (R.Flags _) -> true | _ -> false) "bad flags";
  check (match R.compile "a\xFFb" with Error (R.Invalid_utf8 1) -> true | _ -> false) "a pattern that is not UTF-8";
  check (match R.compile "a\xED\xA0\x80" with Error (R.Invalid_utf8 1) -> true | _ -> false) "a pattern with a surrogate";
  check (R.is_valid ~flags:"u" "\\p{Lu}") "a property with u";
  check (not (R.is_valid ~flags:"u" "\\p{Lu")) "an unterminated property with u";
  check (R.is_valid "\\p{Lu") "Annex B reads \\p as p";
  let deep = String.make 1001 '(' ^ String.make 1001 ')' in
  check (match R.compile deep with Error (R.Too_large { limit = R.Nesting; _ }) -> true | _ -> false) "groups nested too deep";
  check
    (match R.compile "ab)" with
    | Error (R.Invalid m) -> m = "ecmaregex: unmatched ')' at offset 2 in \"ab)\""
    | _ -> false)
    "the syntax error's message";
  check (match R.compile_exn "(" with _ -> false | exception R.Compile_error (R.Invalid _) -> true) "compile_exn raises"

let test_match () =
  let re = compile ~flags:"u" "(?<year>\\d{4})-(?<month>\\d{2})" in
  check (R.is_match re "on 2026-10-25") "is_match over UTF-8";
  check (R.is_match_utf16 re (ascii16 "on 2026-10-25")) "is_match over UTF-16";
  check (not (R.is_match re "on 26-10")) "no match";
  (match R.find re "on 2026-10-25" with
  | Some m ->
      check (R.Match.value m = "2026-10" && R.Match.start m = 3 && R.Match.end_ m = 10) "the match";
      check (R.Match.group m 1 = Some "2026" && R.Match.group m 2 = Some "10" && R.Match.span m 2 = Some (8, 10)) "the groups";
      check (R.Match.named m "month" = Some "10" && R.Match.named m "year" = Some "2026") "the groups by name"
  | None -> check false "the match");
  check (R.group_names re = [ "year"; "month" ] && R.group_count re = 2) "the group names";
  check (List.assoc "month" (R.group_numbers re) = [ 2 ]) "the group of a name";
  check (R.find ~pos:4 re "on 2026-10-25" = None) "a search from a position";
  check
    (match R.find_utf16 ~pos:2 (compile ~flags:"u" "\\d") (ascii16 "a1b2") with Some m -> R.Match.start m = 3 | None -> false)
    "a search from a position over UTF-16";
  check (R.flags_string re = "u") "the flags";
  check (R.pattern re = "(?<year>\\d{4})-(?<month>\\d{2})") "the pattern"

let test_many_names () =
  let pattern = String.concat "" (List.init 200 (fun k -> Printf.sprintf "(?<n%d>a)" (k + 1))) in
  let re = compile pattern in
  check
    (R.group_count re = 200 && List.nth (R.group_names re) 149 = "n150" && List.assoc "n77" (R.group_numbers re) = [ 77 ])
    "two hundred group names"

let test_group_not_taking_part () =
  match R.find (compile "(a)|(b)") "b" with
  | Some m -> check (R.Match.group m 1 = None && R.Match.group m 2 = Some "b") "a group that did not take part"
  | None -> check false "a group that did not take part"

let test_global_search () =
  let all = List.of_seq (R.matches (compile ~flags:"u" "a*") "baa\xF0\x9F\x98\x80") in
  check
    (List.map (fun m -> (R.Match.start m, R.Match.end_ m)) all = [ (0, 0); (1, 3); (3, 3); (7, 7) ])
    "a global search moves on past an empty match";
  check (R.count (compile "\\w+") "hello big world" = 3) "count";
  (* Without the u flag a code unit at a time: between the halves of the pair, and after it. *)
  check (R.count_utf16 (compile "") (u16 [ 0xD83D; 0xDE00 ]) = 3) "empty matches by code unit";
  check (R.count_utf16 (compile ~flags:"u" "") (u16 [ 0xD83D; 0xDE00 ]) = 2) "empty matches by code point"

let test_replace () =
  let re = compile ~flags:"u" "(?<year>\\d{4})-(?<month>\\d{2})" in
  check (R.replace re "2026-10 and 2027-11" "$<month>/$<year>" = "10/2026 and 2027-11") "replace without g replaces the first";
  check
    (R.replace_all re "2026-10 and 2027-11" "[$&|$1|$$|$`|$']"
    = "[2026-10|2026|$|| and 2027-11] and [2027-11|2027|$|2026-10 and |]")
    "the $ forms";
  check (R.replace (compile ~flags:"g" "o") "foo" "0" = "f00") "replace with g";
  (* Without a named group $< is itself. *)
  check (R.replace (compile "o") "o" "$<x>" = "$<x>") "$< without a named group";
  check (R.replace (compile "x") "abc" "y" = "abc") "no match";
  check (R.replace_utf16 (compile "b") (ascii16 "abc") (ascii16 "[$&]") = ascii16 "a[b]c") "over UTF-16"

let test_split () =
  check (R.split (compile "\\d+") "a1b22c" = [ Some "a"; Some "b"; Some "c" ]) "split";
  check
    (R.split (compile "(-)|(\\+)") "a-b+c" = [ Some "a"; Some "-"; None; Some "b"; None; Some "+"; Some "c" ])
    "split with groups";
  check (List.length (R.split ~limit:3 (compile "(-)|(\\+)") "a-b+c") = 3) "split with a limit";
  check (R.split (compile "x*") "" = []) "split of nothing by a match of nothing";
  check (R.split (compile "x") "" = [ Some "" ]) "split of nothing";
  check
    (R.split_utf16 (compile ~flags:"u" "\xF0\x9F\x98\x80") (u16 [ 0x78; 0xD83D; 0xDE00; 0x79 ])
    = [ Some (ascii16 "x"); Some (ascii16 "y") ])
    "split over UTF-16"

let test_refusal () =
  let re = compile ~flags:"u" "a" in
  check (raises_invalid (fun () -> R.is_match re "a\xFF")) "is_match refuses";
  check (raises_invalid (fun () -> R.find re "a\xFF")) "find refuses";
  check (raises_invalid (fun () -> R.is_match re "\xC0\x80")) "an overlong form";
  check (R.utf8_text "\xED\xA0\x80" = Error 0) "a surrogate in UTF-8";
  check (R.is_match_text re (R.utf8_text_exn "xa")) "a checked text";
  check (R.is_match_text re (R.utf8_text_exn ~off:1 ~len:1 "xay")) "a checked range";
  check (not (R.is_match_text re (R.utf8_text_exn ~off:2 "xay"))) "a range holds only its own bytes";
  check (R.utf8_text ~off:1 "a\xFF" = Error 0) "a range that is not UTF-8"

let test_utf16 () =
  let u = [ 0x78; 0xD83D; 0xDE00; 0x79 ] in
  (* A lone low surrogate is a character without the u flag. *)
  check (R.is_match_utf16 (compile "^.y") (u16 [ 0xDE00; 0x79 ])) "a lone surrogate is a character";
  check (R.is_match_utf16 (compile ~flags:"u" "x.y") (u16 u)) "the dot takes a pair with the u flag";
  check (not (R.is_match_utf16 (compile "x.y") (u16 u))) "and one unit without it";
  check
    (match R.find_utf16 (compile ~flags:"u" ".") (u16 [ 0xD83D; 0xDE00 ]) with Some m -> R.Match.end_ m = 2 | None -> false)
    "a pair is one character";
  check
    (match R.find_utf16 (compile ".") (u16 [ 0xD83D; 0xDE00 ]) with Some m -> R.Match.end_ m = 1 | None -> false)
    "a unit is one character";
  check (raises_invalid (fun () -> R.is_match_utf16 (compile "a") "abc")) "UTF-16 of an odd number of bytes"

let test_half_character () =
  (* Without the u flag \ude00 is a code unit, the second half of U+1F600 (F0 9F 98 80 is D83D DE00). *)
  check (R.replace (compile "\\ude00") "a\xF0\x9F\x98\x80b" "-" = "a\xEF\xBF\xBD-b") "a half character is written as U+FFFD";
  check
    (match R.find (compile "\\ude00") "a\xF0\x9F\x98\x80b" with Some m -> R.Match.value m = "\xEF\xBF\xBD" | None -> false)
    "a half character in a match"

let test_timeout () =
  (* A backreference rules out the memory of failed places, so this takes time exponential in the text. *)
  let re = compile ~match_timeout:0.05 "^(?:(a+)\\1*)+$" in
  check (match R.match_timeout re with Some t -> Float.abs (t -. 0.05) < 1e-9 | None -> false) "the timeout";
  check
    (match R.is_match re (String.make 60 'a' ^ "b") with _ -> false | exception R.Match_timeout -> true)
    "a match that runs past the timeout raises Match_timeout";
  check (R.match_timeout (compile "a") = None) "no timeout";
  R.set_default_match_timeout (Some 5.);
  check (R.match_timeout (compile "a") = Some 5.) "the default timeout";
  check (R.match_timeout (compile ~match_timeout:1. "a") = Some 1.) "a timeout of its own over the default";
  check (match R.compile ~match_timeout:2. "(" with Error (R.Invalid _) -> true | _ -> false) "compile with a timeout returns the error";
  R.set_default_match_timeout None;
  check (R.match_timeout (compile "a") = None) "no default timeout"

let test_linear () =
  let text = String.make 5000 'a' ^ "b" in
  check (not (R.is_match (compile "^(a+)+$") text)) "nested quantifiers in linear time";
  check (not (R.is_match (compile "^(?:a|aa)*$") text)) "an alternation in a loop in linear time"

let test_captures () =
  let re = compile ~flags:"u" "(?<k>\\w+)=(?<v>\\d+)" in
  let c = R.captures re in
  let text = R.utf8_text_exn "x a=1 b=22" in
  check (R.exec_into_text re c ~pos:0 text) "exec_into";
  check (R.capture_start c 0 = 2 && R.capture_end c 0 = 5) "the range of the match";
  check (R.capture_start c 2 = 4 && R.capture_end c 2 = 5) "the range of a group";
  let n = ref 0 and pos = ref 0 in
  while R.exec_into_text re c ~pos:!pos text do
    incr n;
    pos := R.capture_end c 0
  done;
  check (!n = 2) "a loop of exec_into";
  check (not (R.exec_into_text re c ~pos:11 text)) "no match from past the end";
  check (R.capture_start c 0 = -1) "no range after no match";
  check
    (R.exec_into_utf16 re c ~pos:0 (ascii16 "k=7") && R.capture_start c 1 = 0 && R.capture_end c 1 = 1)
    "exec_into over UTF-16";
  check (R.exec_into re c ~pos:0 "k=7" && R.capture_start c 2 = 2) "exec_into over a string"

(* Patterns of different sizes share the working memory of a domain: a match of a simple shape, which the search answers
   with no group, after a match that recorded many groups. *)
let test_shared_memory () =
  let many = compile "(a)(b)(c)(d)(e)" and simple = compile "\\d+" in
  check (R.find many "abcde" <> None) "a match of five groups";
  (match R.find simple "x 42" with
  | Some m -> check (R.Match.start m = 2 && R.Match.end_ m = 4 && R.Match.group_count m = 0) "then a simple shape"
  | None -> check false "then a simple shape");
  check (R.find many "abcde" <> None && R.find simple "7" <> None) "and the two again"

let () =
  test_flags ();
  test_compile_errors ();
  test_match ();
  test_many_names ();
  test_group_not_taking_part ();
  test_global_search ();
  test_replace ();
  test_split ();
  test_refusal ();
  test_utf16 ();
  test_half_character ();
  test_timeout ();
  test_linear ();
  test_captures ();
  test_shared_memory ();
  Printf.printf "%d checks, %d failed\n" !checks !failures;
  if !failures > 0 then exit 1
