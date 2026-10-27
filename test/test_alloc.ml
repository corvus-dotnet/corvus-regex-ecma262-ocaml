(* A match allocates nothing once the working memory has grown to what the pattern and the text need: not a test, not a
   count, and not a global search into captures (allocations.rs of the Rust crate, and the Java, Go, Julia and Pascal
   engines' allocation tests). Each case reaches one of the searches for where a match starts.

   The words allocated are counted by Gc.counters, minor and major, which on OCaml 5 count the domain that matches and
   no other. The program prints each failure and a final count, and exits with a status other than zero when anything
   failed. *)

module R = Corvus_regex_ecma262

let failures = ref 0
let checks = ref 0

let check ok what =
  incr checks;
  if not ok then begin
    incr failures;
    print_endline ("FAIL: " ^ what)
  end

(* The words allocated so far, minor and major (a large block goes to the major heap at once). *)
let words () =
  let minor, _, major = Gc.counters () in
  minor +. major

(* What reading the count costs: words () allocates its own tuple and floats. *)
let overhead =
  let least = ref infinity in
  for _ = 1 to 10 do
    let a = words () in
    let b = words () in
    least := Float.min !least (b -. a)
  done;
  !least

exception No_match of string

(* Tests, counts and searches with groups over each text, as a program that matches the same pattern again and again
   does. *)
let work re caps text8 text16 text =
  if not (R.is_match_text re text8 && R.is_match_utf16 re text16 && R.is_match re text) then
    raise (No_match "is_match");
  if R.count_text re text8 = 0 || R.count_utf16 re text16 = 0 || R.count re text = 0 then raise (No_match "count");
  let at = ref 0 in
  while R.exec_into_text re caps ~pos:!at text8 do
    let s = R.capture_start caps 0 and e = R.capture_end caps 0 in
    at := if e = s then s + 1 else e
  done;
  at := 0;
  while R.exec_into_utf16 re caps ~pos:!at text16 do
    let s = R.capture_start caps 0 and e = R.capture_end caps 0 in
    at := if e = s then s + 1 else e
  done;
  if not (R.exec_into re caps ~pos:0 text && R.exec_into_utf16 re caps ~pos:0 text16) then raise (No_match "exec_into")

(* The fewest words allocated by five runs of the work, after two to warm it. *)
let least_words re caps text8 text16 text =
  work re caps text8 text16 text;
  work re caps text8 text16 text;
  let least = ref infinity in
  for _ = 1 to 5 do
    let a = words () in
    work re caps text8 text16 text;
    let b = words () in
    least := Float.min !least (b -. a -. overhead)
  done;
  !least

let piece = "The year 2026, the month 10; Ünïcödé text, 😀 and more letters after it. "

let cases =
  [ "\\d+"; ""; "(?<y>\\d{4})\\D+(\\d+)"; "u"; "text|month"; "i"; "(?<=the )\\w+"; "u"; "(t|e)\\1"; ""; "\\p{L}+"; "u";
    "[\\p{L}--[a-z]]+"; "v"; "^.*$"; "m"; "\\bt\\w*\\b"; "gi";
    (* The searches for where a match starts: a literal, several (packed over UTF-16), the case forms, the windows
       with the reverse automaton, the search after loops, the forward automaton with the skip window, and the shapes
       answered without the matcher. *)
    "letters"; ""; "year|month|text|letters"; ""; "the"; "i"; "year.{0,25}month|month.{0,25}year"; "";
    "\\s[a-z]{0,12}ers\\s"; ""; "\\w+\\s+month"; ""; "\\d\\d:?\\d\\d"; ""; ".*"; "s"; "[^\\n]*"; ""; "\\p{L}"; "u" ]
[@@ocamlformat "disable"]

let utf16_of s =
  let b = Buffer.create (2 * String.length s) and k = ref 0 in
  while !k < String.length s do
    let d = String.get_utf_8_uchar s !k in
    Buffer.add_utf_16le_uchar b (Uchar.utf_decode_uchar d);
    k := !k + Uchar.utf_decode_length d
  done;
  Buffer.contents b

let rec pairs = function a :: b :: rest -> (a, b) :: pairs rest | _ -> []

let () =
  let text = String.concat "" (List.init 20 (fun _ -> piece)) in
  let text8 = R.utf8_text_exn text in
  let text16 = utf16_of text in
  (* The counter counts: a match that builds the strings of its groups allocates. *)
  let re = R.compile_exn "(\\w+) (\\w+)" in
  ignore (R.find re text);
  let a = words () in
  ignore (R.find re text);
  let b = words () in
  check (b -. a -. overhead > 0.) "the counter counts the words of a match that builds strings";
  List.iter
    (fun (pattern, flags) ->
      List.iter
        (fun timeout ->
          (* With no timeout, and with one, whose clock is read as the match goes. *)
          let re = R.compile_exn ~flags ?match_timeout:timeout pattern in
          let caps = R.captures re in
          let least = least_words re caps text8 text16 text in
          check (least <= 0.)
            (Printf.sprintf "/%s/%s with a timeout of %s allocates %.0f words" pattern flags
               (match timeout with None -> "none" | Some s -> Printf.sprintf "%g s" s)
               least))
        [ None; Some 60. ])
    (pairs cases);
  (* Patterns of different sizes taking turns share the domain's working memory, which only grows. *)
  let a = R.compile_exn "(\\w+)\\s+(\\w+)" and b = R.compile_exn "\\d+" and c = R.compile_exn "(a|b)\\1" in
  let turns () =
    ignore (R.is_match_text a text8);
    ignore (R.count_text b text8);
    ignore (R.is_match_text c text8);
    ignore (R.count_utf16 a text16)
  in
  turns ();
  turns ();
  let least = ref infinity in
  for _ = 1 to 5 do
    let x = words () in
    turns ();
    let y = words () in
    least := Float.min !least (y -. x -. overhead)
  done;
  check (!least <= 0.) (Printf.sprintf "patterns taking turns allocate %.0f words" !least);
  Printf.printf "%d checks, %d failed\n" !checks !failures;
  if !failures > 0 then exit 1
