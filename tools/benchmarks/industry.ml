(* The Industry benchmark: each workload counted by each engine, as the V8 harness of the .NET reference engine
   (tools/benchmarks/v8/industry.mjs) counts it, in nanoseconds per count. A port of the Rust crate's
   tools/src/bin/industry.rs (by way of the Julia engine's tools/benchmarks/industry.jl), with the same protocol.

     industry.exe <workloads.json> [--engines corvus-utf8,corvus-utf16,re,pcre2,v8] [--v8 <industry.mjs>]
                  [--rounds 3] [--seconds 0.7] [--passes 7] [--spread 0.05] [--filter <substring of the id>]
                  [--ids <file>] [--out <results.json>]

   The protocol is interleaved, so that what changes on the host over a run (its clock, its heat, its other work) falls
   on every engine alike: the workloads are the outer loop, each workload's rounds next and the engines innermost, their
   order turned by one each round. Each figure is a child process of its own (this program, or Node for V8), so that one
   engine's caches, heap and collector do not reach another's figures; it warms for two fifths of its seconds, then
   times passes passes of as many counts as fit the rest and reports their median. A workload's figure for an engine is
   the median of its rounds, with the fastest and the slowest round as its spread. Where the spread, (slowest - fastest)
   / median, is over --spread, the figure is listed in the run's facts and on stderr. A figure that does not finish in
   30 s is stopped and recorded as giving no result within 30 s, and is not run in the workload's later rounds. A count
   is the global search of ECMA-262: after an empty match the search goes on one character later.

   The engines: this library over UTF-8 (a utf8_text, checked once before the timing) and over UTF-16 (a string of
   UTF-16LE), with the workload's flags and u, counted by count_text and count_utf16 (corvus-utf8, corvus-utf16); re
   (ocaml-re, through its Perl syntax) and pcre2 (the binding to PCRE2, which compiles no JIT code, the text checked once and not at each search), each given the
   pattern as it is and the flags i, m and s, counted by a loop of searches that goes on one character past an empty
   match, and whose figure is an error where the pattern does not compile; and V8 (--v8, the harness run by node), with gu added.

   An engine may name another checkout of this repository as engine@<directory> (corvus-utf8@/tmp/base), which runs that
   engine's figures with that checkout's built harness, so that two versions of the library are timed side by side in
   one interleaved run.

   The texts are those of the reference run, in CORVUS_REGEX_TEXTS (or ~/.local/share/corvus-regex-benchmarks) and
   fetched from dotnet/performance when missing. The run unzips each once into a directory of its own
   (CORVUS_REGEX_PLAIN), which the children read. *)

module R = Corvus_regex_ecma262

let source =
  "https://raw.githubusercontent.com/dotnet/performance/main/src/benchmarks/micro/libraries/"
  ^ "System.Text.RegularExpressions/TestData/"

let default_engines = [ "corvus-utf8"; "corvus-utf16"; "re"; "pcre2" ]

(* A figure stopped at the limit, which the page shows as giving no result within 30 s. *)
let stopped = "{\"error\":\"did not produce a result within 30 s\",\"limit\":30}"

(* What the counts are summed into, so that no count is taken away as dead code. *)
let sink = ref 0

let fail msg =
  prerr_endline ("industry: " ^ msg);
  exit 2

(* A small JSON reader, for the workloads and the children's lines. *)
type json = Null | Bool of bool | Num of float | Str of string | Arr of json list | Obj of (string * json) list

exception Bad_json

let parse_json s =
  let n = String.length s and i = ref 0 in
  let space () =
    while !i < n && (s.[!i] = ' ' || s.[!i] = '\t' || s.[!i] = '\r' || s.[!i] = '\n') do
      incr i
    done
  in
  let expect c = if !i < n && s.[!i] = c then incr i else raise Bad_json in
  let starts lit = !i + String.length lit <= n && String.sub s !i (String.length lit) = lit in
  let hex4 () =
    let v = int_of_string ("0x" ^ String.sub s !i 4) in
    i := !i + 4;
    v
  in
  let str () =
    expect '"';
    let b = Buffer.create 16 in
    let fin = ref false in
    while not !fin do
      if !i >= n then raise Bad_json;
      let c = s.[!i] in
      if c = '"' then begin
        incr i;
        fin := true
      end
      else if c = '\\' then begin
        let e = s.[!i + 1] in
        i := !i + 2;
        match e with
        | 'u' ->
            let u = hex4 () in
            let u =
              if u >= 0xD800 && u <= 0xDBFF && starts "\\u" then begin
                let save = !i in
                i := !i + 2;
                let low = hex4 () in
                if low >= 0xDC00 && low <= 0xDFFF then 0x10000 + ((u - 0xD800) lsl 10) + (low - 0xDC00)
                else begin
                  i := save;
                  u
                end
              end
              else u
            in
            Buffer.add_utf_8_uchar b (if u >= 0xD800 && u <= 0xDFFF then Uchar.rep else Uchar.of_int u)
        | 'n' -> Buffer.add_char b '\n'
        | 't' -> Buffer.add_char b '\t'
        | 'r' -> Buffer.add_char b '\r'
        | 'b' -> Buffer.add_char b '\b'
        | 'f' -> Buffer.add_char b '\012'
        | c -> Buffer.add_char b c
      end
      else begin
        Buffer.add_char b c;
        incr i
      end
    done;
    Buffer.contents b
  in
  let rec value () =
    space ();
    if !i >= n then raise Bad_json;
    match s.[!i] with
    | '{' ->
        incr i;
        space ();
        if !i < n && s.[!i] = '}' then begin
          incr i;
          Obj []
        end
        else begin
          let fields = ref [] and fin = ref false in
          while not !fin do
            space ();
            let k = str () in
            space ();
            expect ':';
            fields := (k, value ()) :: !fields;
            space ();
            if !i < n && s.[!i] = ',' then incr i
            else begin
              expect '}';
              fin := true
            end
          done;
          Obj (List.rev !fields)
        end
    | '[' ->
        incr i;
        space ();
        if !i < n && s.[!i] = ']' then begin
          incr i;
          Arr []
        end
        else begin
          let items = ref [] and fin = ref false in
          while not !fin do
            items := value () :: !items;
            space ();
            if !i < n && s.[!i] = ',' then incr i
            else begin
              expect ']';
              fin := true
            end
          done;
          Arr (List.rev !items)
        end
    | '"' -> Str (str ())
    | _ when starts "true" ->
        i := !i + 4;
        Bool true
    | _ when starts "false" ->
        i := !i + 5;
        Bool false
    | _ when starts "null" ->
        i := !i + 4;
        Null
    | _ ->
        let start = !i in
        while !i < n && String.contains "+-0123456789.eE" s.[!i] do
          incr i
        done;
        if !i = start then raise Bad_json;
        Num (float_of_string (String.sub s start (!i - start)))
  in
  value ()

let member k = function Obj fields -> List.assoc_opt k fields | _ -> None
let str_member k j = match member k j with Some (Str s) -> s | _ -> ""

let json_string s =
  let b = Buffer.create (String.length s + 2) in
  Buffer.add_char b '"';
  String.iter
    (fun c ->
      match c with
      | '"' -> Buffer.add_string b "\\\""
      | '\\' -> Buffer.add_string b "\\\\"
      | '\n' -> Buffer.add_string b "\\n"
      | '\r' -> Buffer.add_string b "\\r"
      | '\t' -> Buffer.add_string b "\\t"
      | c when Char.code c < 0x20 -> Printf.bprintf b "\\u%04x" (Char.code c)
      | c -> Buffer.add_char b c)
    s;
  Buffer.add_char b '"';
  Buffer.contents b

let read_file path = In_channel.with_open_bin path In_channel.input_all
let write_file path s = Out_channel.with_open_bin path (fun oc -> Out_channel.output_string oc s)
let workloads path = match parse_json (read_file path) with Arr l -> Array.of_list l | _ -> fail "workloads.json"

let text_dir () =
  match Sys.getenv_opt "CORVUS_REGEX_TEXTS" with
  | Some d when d <> "" -> d
  | _ -> Filename.concat (Sys.getenv "HOME") ".local/share/corvus-regex-benchmarks"

let plain_name file = if Filename.check_suffix file ".gz" then Filename.chop_suffix file ".gz" else file

let run_command cmd = if Sys.command cmd <> 0 then fail ("failed: " ^ cmd)

(* The output of a command, or "" when it fails. *)
let command_output cmd =
  try
    let ic = Unix.open_process_in (cmd ^ " 2>/dev/null") in
    let s = In_channel.input_all ic in
    ignore (Unix.close_process_in ic);
    s
  with _ -> ""

let first_line s = match String.index_opt s '\n' with Some k -> String.sub s 0 k | None -> s

(* The bytes of a text: gunzipped, without a byte order mark (as unzipped in CORVUS_REGEX_PLAIN). *)
let text file =
  let plain = Option.value (Sys.getenv_opt "CORVUS_REGEX_PLAIN") ~default:"" in
  let p = Filename.concat plain (plain_name file) in
  if plain <> "" && Sys.file_exists p then read_file p
  else begin
    let path = Filename.concat (text_dir ()) file in
    if not (Sys.file_exists path) then begin
      run_command ("mkdir -p " ^ Filename.quote (text_dir ()));
      run_command (Printf.sprintf "curl -sSfL -o %s %s" (Filename.quote path) (Filename.quote (source ^ file)))
    end;
    let b = command_output ("gzip -dc " ^ Filename.quote path) in
    if String.length b >= 3 && String.sub b 0 3 = "\xEF\xBB\xBF" then String.sub b 3 (String.length b - 3) else b
  end

(* The workload's flags with u added, unless it has u or v. *)
let flags_u flags = if String.contains flags 'u' || String.contains flags 'v' then flags else flags ^ "u"

(* UTF-16LE of UTF-8 text. *)
let utf16_of s =
  let b = Buffer.create (2 * String.length s) and k = ref 0 in
  while !k < String.length s do
    let d = String.get_utf_8_uchar s !k in
    Buffer.add_utf_16le_uchar b (Uchar.utf_decode_uchar d);
    k := !k + Uchar.utf_decode_length d
  done;
  Buffer.contents b

(* The width of the UTF-8 character at k, for the step past an empty match. *)
let char_width s k =
  if k >= String.length s then 1 else max 1 (Uchar.utf_decode_length (String.get_utf_8_uchar s k))

let now_ns () = Int64.of_float (Unix.gettimeofday () *. 1e9)

type figures = { median : float; min : float; max : float; count : int; reps : int }

(* Times a count: it warms, then times the passes. *)
let measure seconds passes count =
  let expected = count () in
  let warm_end = Int64.add (now_ns ()) (Int64.of_float (seconds *. 0.4e9)) in
  let reps = ref 0 in
  while Int64.compare (now_ns ()) warm_end < 0 do
    if count () <> expected then failwith "the count changed";
    incr reps
  done;
  let per_pass = max 1 (int_of_float (Float.round (float_of_int !reps *. 0.6 /. float_of_int passes))) in
  let times = Array.make passes 0. in
  for p = 0 to passes - 1 do
    let t0 = now_ns () in
    let s = ref 0 in
    for _ = 1 to per_pass do
      s := !s + count ()
    done;
    times.(p) <- Int64.to_float (Int64.sub (now_ns ()) t0) /. float_of_int per_pass;
    sink := !sink + !s
  done;
  Array.sort compare times;
  { median = times.(passes / 2); min = times.(0); max = times.(passes - 1); count = expected; reps = per_pass * passes }

(* re's flags from the workload's: i, m and s. *)
let re_opts flags =
  List.concat
    [
      (if String.contains flags 'i' then [ `Caseless ] else []);
      (if String.contains flags 'm' then [ `Multiline ] else []);
      (if String.contains flags 's' then [ `Dotall ] else []);
    ]

let re_count re s =
  let n = String.length s and pos = ref 0 and c = ref 0 and fin = ref false in
  while (not !fin) && !pos <= n do
    match Re.exec_opt ~pos:!pos re s with
    | None -> fin := true
    | Some g ->
        incr c;
        let a, e = Re.Group.offset g 0 in
        pos := if e = a then e + char_width s e else e
  done;
  !c

let pcre2_flags flags =
  List.concat
    [
      [ `UTF ];
      (if String.contains flags 'i' then [ `CASELESS ] else []);
      (if String.contains flags 'm' then [ `MULTILINE ] else []);
      (if String.contains flags 's' then [ `DOTALL ] else []);
    ]

(* The text is checked as UTF-8 once before the timing (as this library's utf8_text is), so PCRE2 is told not to check
   it again at each search, which would read the whole text each time. *)
let pcre2_no_check = Pcre2.rflags [ `NO_UTF_CHECK ]

let pcre2_count rex s =
  let n = String.length s and pos = ref 0 and c = ref 0 and fin = ref false in
  while (not !fin) && !pos <= n do
    match Pcre2.exec ~iflags:pcre2_no_check ~rex ~pos:!pos s with
    | exception Not_found -> fin := true
    | sub ->
        incr c;
        let a, e = Pcre2.get_substring_ofs sub 0 in
        pos := if e = a then e + char_width s e else e
  done;
  !c

(* Runs one workload on one engine in this process and prints its figures as one line of JSON. *)
let one path index engine seconds passes =
  let w = (workloads path).(index) in
  let b = text (str_member "file" w) in
  let pattern = str_member "pattern" w and flags = str_member "flags" w in
  match
    match engine with
    | "corvus-utf8" ->
        let t = R.utf8_text_exn b and re = R.compile_exn ~flags:(flags_u flags) pattern in
        measure seconds passes (fun () -> R.count_text re t)
    | "corvus-utf16" ->
        let u = utf16_of b and re = R.compile_exn ~flags:(flags_u flags) pattern in
        measure seconds passes (fun () -> R.count_utf16 re u)
    | "re" ->
        let re = Re.compile (Re.Perl.re ~opts:(re_opts flags) pattern) in
        measure seconds passes (fun () -> re_count re b)
    | "pcre2" ->
        if not (String.is_valid_utf_8 b) then failwith "the text is not UTF-8";
        let rex = Pcre2.regexp ~flags:(pcre2_flags flags) pattern in
        measure seconds passes (fun () -> pcre2_count rex b)
    | _ -> failwith ("unknown engine " ^ engine)
  with
  | f ->
      Printf.printf "{\"median\":%.1f,\"min\":%.1f,\"max\":%.1f,\"count\":%d,\"reps\":%d}\n%!" f.median f.min f.max
        f.count f.reps
  | exception e -> Printf.printf "{\"error\":%s}\n%!" (json_string (first_line (Printexc.to_string e)))

(* Reads a line from fd, waiting no later than deadline (seconds since the epoch); None at the deadline. *)
let read_line_until fd deadline =
  let b = Buffer.create 256 and chunk = Bytes.create 4096 and result = ref None and fin = ref false in
  while not !fin do
    let left = deadline -. Unix.gettimeofday () in
    if left <= 0. then fin := true
    else
      match Unix.select [ fd ] [] [] left with
      | [], _, _ -> ()
      | _ -> (
          match Unix.read fd chunk 0 (Bytes.length chunk) with
          | 0 ->
              result := Some (first_line (Buffer.contents b));
              fin := true
          | k ->
              Buffer.add_subbytes b chunk 0 k;
              if Bytes.index_opt (Bytes.sub chunk 0 k) '\n' <> None then begin
                result := Some (first_line (Buffer.contents b));
                fin := true
              end
          | exception Unix.Unix_error (Unix.EINTR, _, _) -> ())
      | exception Unix.Unix_error (Unix.EINTR, _, _) -> ()
  done;
  !result

(* Runs engine over workload index in a child process, and returns its line of JSON, or one with the error. *)
let figure v8 path index engine seconds passes =
  let args =
    if engine = "v8" then [| "node"; v8; "--one"; path; string_of_int index; seconds; passes |]
    else
      match String.index_opt engine '@' with
      | Some k ->
          (* Another checkout of this repository, whose built harness runs the figure. *)
          let name = String.sub engine 0 k and checkout = String.sub engine (k + 1) (String.length engine - k - 1) in
          let exe = Filename.concat checkout "_build/default/tools/benchmarks/industry.exe" in
          [| exe; "--one"; path; string_of_int index; name; seconds; passes |]
      | None -> [| Sys.executable_name; "--one"; path; string_of_int index; engine; seconds; passes |]
  in
  let out_r, out_w = Unix.pipe ~cloexec:true () in
  let devnull = Unix.openfile "/dev/null" [ Unix.O_RDONLY; Unix.O_CLOEXEC ] 0 in
  let pid = Unix.create_process args.(0) args devnull out_w Unix.stderr in
  Unix.close out_w;
  Unix.close devnull;
  (* The watchdog: a child that gave its line ends by itself, and one that runs past the limit is killed outright. *)
  let line = read_line_until out_r (Unix.gettimeofday () +. 30.) in
  (match line with None -> ( try Unix.kill pid Sys.sigkill with _ -> ()) | Some _ -> ());
  ignore (Unix.waitpid [] pid);
  Unix.close out_r;
  match line with
  | None -> stopped
  | Some "" -> "{\"error\":\"the process ended without figures\"}"
  | Some l -> String.trim l

let median_of sorted =
  let n = Array.length sorted in
  if n mod 2 = 1 then sorted.(n / 2) else (sorted.((n / 2) - 1) +. sorted.(n / 2)) /. 2.

let field file prefix =
  if not (Sys.file_exists file) then ""
  else
    In_channel.with_open_text file (fun ic ->
        let rec go () =
          match In_channel.input_line ic with
          | None -> ""
          | Some l ->
              if String.length l >= String.length prefix && String.sub l 0 (String.length prefix) = prefix then
                match String.index_opt l ':' with
                | Some k -> String.trim (String.sub l (k + 1) (String.length l - k - 1))
                | None -> ""
              else go ()
        in
        go ())

let env k = Option.value (Sys.getenv_opt k) ~default:""

(* The version of an opam package of the harness: from the image (which records them when it is built), or from opam. *)
let package_version var name =
  match env var with "" -> String.trim (command_output ("opam show -f version " ^ name)) | v -> v

(* The version of the PCRE2 library: the binding's string is the C buffer it was read into, padded past the version. *)
let pcre2_library_version =
  match String.index_opt Pcre2.version '\000' with Some k -> String.sub Pcre2.version 0 k | None -> Pcre2.version

(* What the run is: the host, the runtime, the affinity, the load and the commit, read from the run. *)
let facts seconds passes extra =
  let load = match String.split_on_char ' ' (try read_file "/proc/loadavg" with _ -> "") with l :: _ -> l | [] -> "" in
  let commit = match env "CORVUS_COMMIT" with "" -> first_line (command_output "git rev-parse HEAD") | c -> c in
  let flambda = if Build_info.flambda then " flambda" else "" in
  let f =
    [
      ("cpu", field "/proc/cpuinfo" "model name");
      ("cores", String.trim (command_output "nproc --all"));
      ("affinity", field "/proc/self/status" "Cpus_allowed_list");
      ("os", first_line (command_output "uname -sr"));
      ("load", load);
      ("ocaml", Printf.sprintf "OCaml %s%s %s" Sys.ocaml_version flambda (first_line (command_output "uname -m")));
      ("commit", commit);
      ("date", first_line (command_output "date -u +%Y-%m-%dT%H:%M:%SZ"));
      ("seconds", seconds);
      ("passes", passes);
      (* On a GitHub-hosted runner, what the runner says it is. *)
      ("runner", env "RUNNER_NAME");
      ("runner_os", env "RUNNER_OS");
      ("image", String.trim (env "ImageOS" ^ " " ^ env "ImageVersion"));
      ("corvus-regex-ecma262", R.version);
      ("re", "re " ^ package_version "RE_VERSION" "re");
      ( "pcre2",
        Printf.sprintf "PCRE2 %s (pcre2 %s)" pcre2_library_version (package_version "PCRE2_OCAML_VERSION" "pcre2") );
    ]
    @ extra
  in
  "{" ^ String.concat "," (List.map (fun (k, v) -> json_string k ^ ":" ^ json_string v) f) ^ "}"

let count_of line =
  match parse_json line with
  | j -> ( match member "count" j with Some (Num c) -> Some (int_of_float c) | _ -> None)
  | exception _ -> None

let contains s sub =
  let n = String.length sub in
  let rec at k = k + n <= String.length s && (String.sub s k n = sub || at (k + 1)) in
  n = 0 || at 0

let main args =
  match args with
  | "--one" :: path :: index :: engine :: seconds :: passes :: _ ->
      one path (int_of_string index) engine (float_of_string seconds) (int_of_string passes)
  | [] -> fail "usage: industry.exe <workloads.json> [--engines ...] [--v8 <industry.mjs>] [--out <file>]"
  | path :: rest ->
      let rec options acc = function k :: v :: rest -> options ((k, v) :: acc) rest | _ -> acc in
      let opt = options [] rest in
      let get k d = Option.value (List.assoc_opt k opt) ~default:d in
      let v8 = get "--v8" "" in
      let engines =
        match List.assoc_opt "--engines" opt with
        | Some e -> String.split_on_char ',' e
        | None -> if v8 <> "" then default_engines @ [ "v8" ] else default_engines
      in
      if List.mem "v8" engines && v8 = "" then fail "V8 is run by the V8 harness: give --v8 <industry.mjs>";
      let rounds = int_of_string (get "--rounds" "3") in
      let seconds = get "--seconds" "0.7" and passes = get "--passes" "7" in
      let limit = float_of_string (get "--spread" "0.05") in
      let filter = get "--filter" "" in
      (* One id a line: the ids hold spaces. *)
      let ids =
        match List.assoc_opt "--ids" opt with
        | Some f ->
            Some (List.filter (( <> ) "") (List.map String.trim (String.split_on_char '\n' (read_file f))))
        | None -> None
      in
      let all = workloads path in
      let chosen =
        List.filter
          (fun k ->
            let id = str_member "id" all.(k) in
            contains id filter && match ids with None -> true | Some l -> List.mem id l)
          (List.init (Array.length all) Fun.id)
      in
      let plain = env "CORVUS_REGEX_PLAIN" in
      if plain = "" then fail "run.sh sets CORVUS_REGEX_PLAIN, the directory of the unzipped texts";
      (* The texts unzipped once, for every child. *)
      run_command ("mkdir -p " ^ Filename.quote plain);
      Array.iter
        (fun w ->
          let file = str_member "file" w in
          let p = Filename.concat plain (plain_name file) in
          if not (Sys.file_exists p) then write_file p (text file))
        all;
      let engines = Array.of_list engines in
      let ne = Array.length engines in
      let times = Array.init ne (fun _ -> Array.make (Array.length all) []) in
      let lines = Array.init ne (fun _ -> Array.make (Array.length all) "") in
      let names = Hashtbl.create 4 in
      List.iteri
        (fun done_ k ->
          for round = 0 to rounds - 1 do
            for turn = 0 to ne - 1 do
              let e = (turn + round) mod ne in
              (* A figure stopped at the limit in an earlier round is not run again. *)
              if lines.(e).(k) <> stopped then begin
                let line = figure v8 path k engines.(e) seconds passes in
                let parsed = try parse_json line with _ -> Obj [] in
                (match member "median" parsed with
                | Some (Num m) -> times.(e).(k) <- m :: times.(e).(k)
                | _ -> Printf.eprintf "%-13s %-62s %s\n%!" engines.(e) (str_member "id" all.(k)) line);
                (match member "engine" parsed with Some (Str n) -> Hashtbl.replace names engines.(e) n | _ -> ());
                lines.(e).(k) <- line
              end
            done
          done;
          Printf.eprintf "workload %d of %d done\n%!" (done_ + 1) (List.length chosen))
        chosen;
      let over = ref [] and differ = ref [] in
      (* V8 is the oracle of the counts: an engine that counts otherwise has given a wrong answer, whatever its
         dialect. *)
      let v8e =
        let r = ref None in
        Array.iteri (fun e n -> if n = "v8" then r := Some e) engines;
        !r
      in
      let oracle =
        Array.mapi
          (fun k w ->
            match v8e with
            | Some e -> count_of lines.(e).(k)
            | None -> ( match member "v8Count" w with Some (Num c) -> Some (int_of_float c) | _ -> None))
          all
      in
      let results =
        Array.to_list
          (Array.mapi
             (fun e engine ->
               let rows =
                 List.map
                   (fun k ->
                     let line = lines.(e).(k) and ts = times.(e).(k) in
                     let id = str_member "id" all.(k) in
                     let entry =
                       if List.length ts < rounds then
                         (* A round with no figure (an error, a limit) makes the figure an error. *)
                         if contains line "\"error\"" then line else "{\"error\":\"a round gave no figure\"}"
                       else begin
                         let sorted = Array.of_list ts in
                         Array.sort compare sorted;
                         let m = median_of sorted and lo = sorted.(0) and hi = sorted.(Array.length sorted - 1) in
                         let spread = if m > 0. then (hi -. lo) /. m else 0. in
                         if spread > limit then begin
                           over := Printf.sprintf "%s %s %.1f%%" engine id (spread *. 100.) :: !over;
                           Printf.eprintf "SPREAD %-13s %-62s %.1f%% over %.1f%%\n%!" engine id (spread *. 100.)
                             (limit *. 100.)
                         end;
                         let count = Option.value (count_of line) ~default:0 in
                         match oracle.(k) with
                         | Some o when o <> count && engine <> "v8" ->
                             differ := Printf.sprintf "%s %s %d vs V8 %d" engine id count o :: !differ;
                             Printf.eprintf "COUNT  %-13s %-62s %d against V8's %d\n%!" engine id count o;
                             "{\"error\":" ^ json_string (Printf.sprintf "counted %d where V8 counts %d" count o) ^ "}"
                         | _ ->
                             Printf.sprintf
                               "{\"median\":%.1f,\"min\":%.1f,\"max\":%.1f,\"count\":%d,\"rounds\":%d,\"spread\":%.4f,\"each\":[%s]}"
                               m lo hi count (List.length ts) spread
                               (String.concat "," (List.rev_map (Printf.sprintf "%.1f") ts))
                       end
                     in
                     json_string id ^ ":" ^ entry)
                   chosen
               in
               json_string engine ^ ":{" ^ String.concat "," rows ^ "}")
             engines)
      in
      let extra =
        [
          ("rounds", string_of_int rounds);
          ("protocol", "interleaved");
          ("spread_limit", get "--spread" "0.05");
          ("spread_over", String.concat "; " (List.rev !over));
          ("counts_differ", String.concat "; " (List.rev !differ));
          ("v8", Option.value (Hashtbl.find_opt names "v8") ~default:"");
        ]
      in
      let end_ = first_line (command_output "date -u +%Y-%m-%dT%H:%M:%SZ") in
      let out =
        "{\"facts\":" ^ facts seconds passes extra ^ ",\"end\":" ^ json_string end_ ^ ",\"results\":{"
        ^ String.concat "," results ^ "}}\n"
      in
      match List.assoc_opt "--out" opt with Some f -> write_file f out | None -> print_string out

let () = main (List.tl (Array.to_list Sys.argv))
