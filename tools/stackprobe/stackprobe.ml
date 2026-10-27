(* Asks the engine about patterns built to overflow the stack: deep (groups, lookarounds, modifier groups, quantified
   groups and the classes of the v flag, each nested to the limit, one past it and far past it), wide (many groups,
   classes, alternatives, backreferences, set operations and string alternatives side by side) and long (the longest
   pattern allowed, and one past it), in every grammar, through every call over UTF-8 and UTF-16, as the Rust crate's
   stack-probe and the Pascal engine's tools/stackprobe do.

     stackprobe.exe [--filter text]

   A probe is answered (compiled and matched), refused (not valid, or too large, which a caller can catch), or a fault:
   any other exception, Stack_overflow among them. A stack that overflows past what the runtime can catch ends the
   process, which is a fault too: the exit status is not 0 either way. The stack is the one the process runs with: the
   system stack on OCaml 4.14 (ulimit -s) and the stack limit of OCAMLRUNPARAM (l) on OCaml 5, whose stacks grow on the
   heap up to it; tools/stackprobe/run.sh runs the probes under two of each. *)

module R = Corvus_regex_ecma262

type probe = { name : string; flags : string; pattern : string; text : string }

let probes = ref []
let add name flags pattern text = probes := { name; flags; pattern; text } :: !probes

let rep s n =
  let b = Buffer.create (String.length s * n) in
  for _ = 1 to n do
    Buffer.add_string b s
  done;
  Buffer.contents b

let nest op body close depth = rep op depth ^ body ^ rep close depth
let text_of n = rep "a" (min (1 lsl 20) (max 64 n)) ^ "b"

let named depth =
  let b = Buffer.create (8 * depth) in
  for k = 0 to depth - 1 do
    Printf.bprintf b "(?<g%d>" k
  done;
  Buffer.contents b ^ "a" ^ rep ")" depth

let named_side width =
  let b = Buffer.create (10 * width) in
  for k = 0 to width - 1 do
    Printf.bprintf b "(?<g%d>a)" k
  done;
  Buffer.contents b

let build () =
  let depths = [ 32; 33; 200; 1000; 1001; 5000; 200000 ] in
  let grammars = [ ""; "u"; "v"; "i"; "iv" ] in
  let groups =
    [ ("group", "(", ")"); ("non-capturing", "(?:", ")"); ("lookahead", "(?=", ")"); ("negative lookahead", "(?!", ")");
      ("lookbehind", "(?<=", ")"); ("negative lookbehind", "(?<!", ")"); ("modifier i", "(?i:", ")");
      ("modifier -i", "(?-i:", ")"); ("star", "(?:", ")*"); ("counted", "(?:", "){2}"); ("lazy plus", "(?:", ")+?");
      ("alternative", "(?:b|", ")") ]
  [@@ocamlformat "disable"]
  in
  let classes =
    [ ("class", "[", "a", "]"); ("negated class", "[^", "a", "]"); ("difference", "[a--", "[b]", "]");
      ("intersection", "[a&&", "[a]", "]"); ("string alternatives", "[\\q{ab|c}", "a", "]");
      ("class in a group", "(?:[", "a", "])") ]
  [@@ocamlformat "disable"]
  in
  List.iter
    (fun (name, op, close) ->
      List.iter
        (fun d ->
          List.iter
            (fun f -> add (Printf.sprintf "deep %s x%d /%s" name d f) f (nest op "a" close d) (text_of d))
            grammars)
        depths)
    groups;
  List.iter
    (fun d ->
      List.iter
        (fun f ->
          add (Printf.sprintf "deep named x%d /%s" d f) f (named d) (text_of d);
          add (Printf.sprintf "deep backreference x%d /%s" d f) f (nest "(" "a" ")\\1" d) (text_of d))
        grammars)
    depths;
  List.iter
    (fun (name, op, body, close) ->
      List.iter
        (fun d ->
          List.iter
            (fun f -> add (Printf.sprintf "deep %s x%d /%s" name d f) f (nest op body close d) (text_of d))
            grammars)
        depths)
    classes;
  List.iter
    (fun d ->
      add (Printf.sprintf "deep [ unclosed x%d /v" d) "v" (rep "[" d) (text_of 64);
      add (Printf.sprintf "deep ( unclosed x%d /v" d) "v" (rep "(" d) (text_of 64))
    depths;
  List.iter
    (fun w ->
      List.iter
        (fun f ->
          let a name pattern = add (Printf.sprintf "wide %s x%d /%s" name w f) f pattern (text_of w) in
          a "alternatives" (rep "a|" w ^ "b");
          a "empty groups" (rep "()" w);
          a "groups" (rep "(a)" w);
          a "named groups" (named_side w);
          a "classes" (rep "[a]" w);
          a "lookaheads" (rep "(?=a)" w ^ "a");
          a "backreferences" ("(a)" ^ rep "\\1" w);
          a "optional" (rep "a?" w);
          a "optional groups" (rep "(?:a?)" w);
          a "ranges in a class" ("[" ^ rep "a-b" w ^ "]");
          a "properties" (rep "\\p{L}" w);
          a "stars" (rep "a*" w);
          a "alternatives in a star" ("(?:" ^ rep "a|" w ^ "b)*");
          a "differences" ("[a" ^ rep "--b" w ^ "]");
          a "intersections" ("[a" ^ rep "&&a" w ^ "]");
          a "string alternatives" ("[\\q{" ^ rep "a|" w ^ "b}]"))
        grammars)
    [ 1000; 100000; 300000 ];
  List.iter
    (fun f ->
      add (Printf.sprintf "long literal x%d /%s" (1 lsl 20) f) f (rep "a" (1 lsl 20)) (text_of (1 lsl 20));
      add (Printf.sprintf "long literal x%d /%s" ((1 lsl 20) + 1) f) f (rep "a" ((1 lsl 20) + 1))
        (text_of ((1 lsl 20) + 1)))
    grammars;
  List.rev !probes

let ascii16 s =
  let b = Bytes.create (2 * String.length s) in
  String.iteri (fun k c -> Bytes.set_uint16_le b (2 * k) (Char.code c)) s;
  Bytes.to_string b

(* Everything the engine is asked about one probe; the answer is a line. *)
let ask p =
  let valid = R.is_valid ~flags:p.flags p.pattern in
  match R.compile ~flags:p.flags ~match_timeout:5.0 p.pattern with
  | Error (R.Too_large _) ->
      if valid then "FAULT is_valid accepted a pattern too large to compile" else "refused too large"
  | Error e ->
      if valid then "FAULT is_valid accepted a pattern the compiler refused: " ^ R.error_message e
      else "refused not valid"
  | Ok re ->
      if not valid then "FAULT is_valid refused a pattern the compiler accepted"
      else begin
        let u = ascii16 p.text in
        let answers = Buffer.create 64 in
        let answer f =
          Buffer.add_char answers ' ';
          Buffer.add_string answers
            (try f () with R.Match_timeout -> "timeout" | R.Match_memory -> "memory")
        in
        let start = function Some m -> string_of_int (R.Match.start m) | None -> "-1" in
        answer (fun () -> string_of_bool (R.is_match re p.text));
        answer (fun () -> string_of_bool (R.is_match_utf16 re u));
        answer (fun () -> start (R.find re p.text));
        answer (fun () -> start (R.find_utf16 re u));
        answer (fun () -> string_of_int (Seq.length (R.matches re p.text)));
        answer (fun () -> string_of_int (Seq.length (R.matches_utf16 re u)));
        answer (fun () -> string_of_int (R.count re p.text));
        answer (fun () -> string_of_bool (R.exec_into re (R.captures re) ~pos:0 p.text));
        answer (fun () -> string_of_int (String.length (R.replace_all re p.text "$1x")));
        answer (fun () -> string_of_int (String.length (R.replace_all_utf16 re u (ascii16 "x"))));
        answer (fun () -> string_of_int (List.length (R.split re p.text)));
        answer (fun () -> string_of_int (List.length (R.split_utf16 re u)));
        "answered" ^ Buffer.contents answers
      end

let () =
  let filter = ref "" in
  Arg.parse [ ("--filter", Arg.Set_string filter, "only the probes whose name holds this") ] ignore "stackprobe";
  let contains s sub =
    let n = String.length sub in
    let rec at k = k + n <= String.length s && (String.sub s k n = sub || at (k + 1)) in
    n = 0 || at 0
  in
  let all = build () in
  let started = Unix.gettimeofday () in
  let answered = ref 0 and refused = ref 0 and faults = ref [] in
  List.iter
    (fun p ->
      if contains p.name !filter then begin
        let r = try ask p with Stack_overflow -> "FAULT Stack_overflow" | e -> "FAULT " ^ Printexc.to_string e in
        if String.length r >= 8 && String.sub r 0 8 = "answered" then incr answered
        else if String.length r >= 7 && String.sub r 0 7 = "refused" then incr refused
        else faults := (p.name ^ ": " ^ r) :: !faults
      end)
    all;
  List.iter print_endline (List.sort compare !faults);
  Printf.printf "%d probes: %d answered, %d refused, %d faults, in %.0f s\n" (List.length all) !answered !refused
    (List.length !faults)
    (Unix.gettimeofday () -. started);
  if !faults <> [] then exit 1
