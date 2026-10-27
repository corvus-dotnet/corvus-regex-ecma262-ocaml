(* ECMA-262 regular expressions over UTF-8 and UTF-16 (regex.rs, replace.rs and lib.rs of the Rust crate). See the
   interface for what each member does. *)

let version = "0.1.0"
let unicode_version = Ucd_tables.version

type flags = {
  has_indices : bool;
  global : bool;
  ignore_case : bool;
  multiline : bool;
  dot_all : bool;
  unicode : bool;
  unicode_sets : bool;
  sticky : bool;
}

let no_flags =
  {
    has_indices = false;
    global = false;
    ignore_case = false;
    multiline = false;
    dot_all = false;
    unicode = false;
    unicode_sets = false;
    sticky = false;
  }

type limit = Length | Nesting | Size

type error =
  | Invalid of string
  | Flags of string
  | Too_large of { limit : limit; value : int }
  | Invalid_utf8 of int

let limit_name = function Length -> "length" | Nesting -> "nesting" | Size -> "size"

let error_message = function
  | Invalid m | Flags m -> m
  | Too_large { limit; value } ->
      Parse.too_large_message (match limit with Length -> Parse.Length | Nesting -> Parse.Nesting | Size -> Parse.Size) value
  | Invalid_utf8 k -> Printf.sprintf "the pattern is not UTF-8 at byte %d" k

exception Compile_error of error
exception Match_timeout
exception Match_memory

let max_pattern_length = Parse.max_pattern_length
let max_nesting = Parse.max_nesting
let max_size = Parse.max_size
let max_stack = Vm_state.max_stack

let parse_flags s =
  let f = ref no_flags and bad = ref None in
  String.iter
    (fun c ->
      if !bad = None then begin
        let seen, set =
          match c with
          | 'd' -> (!f.has_indices, fun () -> f := { !f with has_indices = true })
          | 'g' -> (!f.global, fun () -> f := { !f with global = true })
          | 'i' -> (!f.ignore_case, fun () -> f := { !f with ignore_case = true })
          | 'm' -> (!f.multiline, fun () -> f := { !f with multiline = true })
          | 's' -> (!f.dot_all, fun () -> f := { !f with dot_all = true })
          | 'u' -> (!f.unicode, fun () -> f := { !f with unicode = true })
          | 'v' -> (!f.unicode_sets, fun () -> f := { !f with unicode_sets = true })
          | 'y' -> (!f.sticky, fun () -> f := { !f with sticky = true })
          | _ -> (false, fun () -> bad := Some (Printf.sprintf "invalid flag %C in %S" c s))
        in
        if seen then bad := Some (Printf.sprintf "the flag %C is given twice in %S" c s) else set ()
      end)
    s;
  match !bad with
  | Some m -> Error (Flags m)
  | None ->
      if !f.unicode && !f.unicode_sets then Error (Flags (Printf.sprintf "the flags u and v are given together in %S" s))
      else Ok !f

let flags_to_string f =
  String.concat ""
    (List.filter_map
       (fun (on, c) -> if on then Some c else None)
       [
         (f.has_indices, "d");
         (f.global, "g");
         (f.ignore_case, "i");
         (f.multiline, "m");
         (f.dot_all, "s");
         (f.unicode, "u");
         (f.unicode_sets, "v");
         (f.sticky, "y");
       ])

let syntax f : Parse.syntax =
  {
    unicode = f.unicode || f.unicode_sets;
    sets = f.unicode_sets;
    icase = f.ignore_case;
    multiline = f.multiline;
    dot_all = f.dot_all;
  }

(* The match timeout every pattern compiled without one of its own takes, in nanoseconds; 0 for none. *)
let default_timeout = Atomic.make 0
let ns_of_seconds s = if s <= 0. then 0 else if s >= 1e9 then max_int else int_of_float (s *. 1e9)
let set_default_match_timeout t = Atomic.set default_timeout (match t with None -> 0 | Some s -> ns_of_seconds s)

let default_match_timeout () =
  match Atomic.get default_timeout with 0 -> None | ns -> Some (float_of_int ns /. 1e9)

(* A group name in both encodings, and the groups that have it (more than one only in separate alternatives). *)
type group_name = { name : string; name16 : string; groups : int array }

type t = {
  pattern : string;
  flags : flags;
  (* The program for a match that needs no groups: it records only the groups some backreference names. *)
  prog : Prog.t;
  (* The program that records every group, built the first time a match asks for the groups. *)
  caps : Prog.t option Atomic.t;
  (* The automaton of the pattern over ASCII text (see Dfa), built on the first match that wants it, for a pattern that
     needs no backtracking and is small enough to be tried for one; None for any other. *)
  dfa : Dfa.t option Once.t option;
  group_count : int;
  names : group_name list;
  (* The time a match may take in nanoseconds, or 0 for no limit. *)
  timeout : int;
  (* The working memory a match takes when the caller gives none: one match at a time holds it, and a match that finds
     it held makes its own. *)
  state : Vm_state.t option Atomic.t;
  (* The pattern's own working memory, made once: state holds it (this very option) when no match holds it, so that it
     is taken and given back without allocating. *)
  home : Vm_state.t option;
  (* The text last made for a string of UTF-8 and of UTF-16: a string is immutable, so the same string again is the same
     text, checked already, and no new one is made. *)
  last8 : Units.text Atomic.t;
  last16 : Units.text Atomic.t;
}

(* No text: what the pattern's text of the last string starts as. *)
let no_text = { Units.s = ""; base = -1; len = 0 }

let utf8_of_runes runes =
  let b = Buffer.create (Array.length runes) in
  Array.iter (fun r -> Compile.utf8_of_rune b (if r >= 0xD800 && r <= 0xDFFF then 0xFFFD else r)) runes;
  Buffer.contents b

let utf16_of_runes runes =
  let b = Buffer.create (2 * Array.length runes) in
  Array.iter (Compile.utf16_of_rune b) runes;
  Buffer.contents b

let too_large limit value =
  Too_large { limit = (match limit with Parse.Length -> Length | Parse.Nesting -> Nesting | Parse.Size -> Size); value }

let invalid pattern msg pos = Invalid (Printf.sprintf "ecmaregex: %s at offset %d in %S" msg pos pattern)

let compile_with_ns pattern flags timeout =
  let k = Units.first_invalid_utf8 pattern in
  if k >= 0 then Error (Invalid_utf8 k)
  else
    match Parse.parse_with pattern (syntax flags) with
    | exception Parse.Syntax (msg, pos) -> Error (invalid pattern msg pos)
    | exception Parse.Too_large (limit, value) -> Error (too_large limit value)
    | root, ps ->
        let names =
          List.map
            (fun (name, groups) -> { name = utf8_of_runes name; name16 = utf16_of_runes name; groups })
            (Parse.group_names ps)
        in
        let prog = Prog.make root (flags.unicode || flags.unicode_sets) None in
        let regular = (not ps.Parse.needs_backtracking) && Dfa.re2_cost root <= Dfa.max_re2_cost in
        let dfa =
          if regular then
            (* Built from the pattern read again, so that a pattern does not keep its tree. *)
            Some
              (Once.make (fun () ->
                   match Parse.parse_with pattern (syntax flags) with root, _ -> Dfa.build root | exception _ -> None))
          else None
        in
        let home = Some (Vm_state.create ()) in
        Ok
          {
            pattern;
            flags;
            prog;
            caps = Atomic.make None;
            dfa;
            group_count = Parse.group_count ps;
            names;
            timeout;
            state = Atomic.make home;
            home;
            last8 = Atomic.make no_text;
            last16 = Atomic.make no_text;
          }

let compile ?(flags = "") ?match_timeout pattern =
  match parse_flags flags with
  | Error e -> Error e
  | Ok f ->
      let ns = match match_timeout with None -> Atomic.get default_timeout | Some s -> ns_of_seconds s in
      compile_with_ns pattern f ns

let compile_exn ?flags ?match_timeout pattern =
  match compile ?flags ?match_timeout pattern with Ok re -> re | Error e -> raise (Compile_error e)

let is_valid ?(flags = "") pattern =
  match parse_flags flags with
  | Error _ -> false
  | Ok f -> (
      Units.first_invalid_utf8 pattern < 0
      && match Parse.parse_with pattern (syntax f) with exception _ -> false | _ -> true)

let pattern re = re.pattern
let flags re = re.flags
let flags_string re = flags_to_string re.flags
let match_timeout re = if re.timeout = 0 then None else Some (float_of_int re.timeout /. 1e9)
let group_count re = re.group_count
let group_names re = List.map (fun n -> n.name) re.names
let group_numbers re = List.map (fun n -> (n.name, Array.to_list n.groups)) re.names
let unicode re = re.flags.unicode || re.flags.unicode_sets

(* The program that records every group. *)
let caps_program re =
  if re.prog.p.groups = re.group_count then re.prog
  else
    match Atomic.get re.caps with
    | Some p -> p
    | None ->
        let root, _ = Parse.parse_with re.pattern (syntax re.flags) in
        let p = Prog.make root (unicode re) (Some re.group_count) in
        ignore (Atomic.compare_and_set re.caps None (Some p));
        p

(* The working memory of a match: the domain's (OCaml 5), the pattern's when no other match holds it, or a fresh one. *)
let take_state re =
  match Domain_home.take () with
  | Some st -> st
  | None -> (
      match re.home with
      | Some home when Atomic.compare_and_set re.state re.home None -> home
      | _ -> Vm_state.create ())

(* Gives the working memory back to the domain or the pattern it came from. *)
let release_state re st =
  Domain_home.give_back st;
  match re.home with Some home when home == st -> Atomic.set re.state re.home | _ -> ()

let raise_stopped code = if code = Vm_state.stopped_time then raise Match_timeout else raise Match_memory

(* The text of a string of UTF-8, checked. *)
let utf8_text_of s =
  let k = Units.first_invalid_utf8 s in
  if k >= 0 then invalid_arg (Printf.sprintf "Corvus_regex_ecma262: the text is not UTF-8 at byte %d" k);
  { Units.s; base = 0; len = String.length s }

let utf16_text_of s =
  if String.length s land 1 <> 0 then invalid_arg "Corvus_regex_ecma262: UTF-16 text has an odd number of bytes";
  { Units.s; base = 0; len = String.length s / 2 }

(* The text of a whole string, the one last made for it where it is the same string. *)
let text_of (last : Units.text Atomic.t) make s =
  let t = Atomic.get last in
  if t.s == s && t.base = 0 then t
  else begin
    let t = make s in
    Atomic.set last t;
    t
  end

let text8 re s = text_of re.last8 utf8_text_of s
let text16 re s = text_of re.last16 utf16_text_of s

type utf8_text = Units.text

let utf8_text ?(off = 0) ?len s =
  let len = match len with Some l -> l | None -> String.length s - off in
  if off < 0 || len < 0 || off + len > String.length s then invalid_arg "Corvus_regex_ecma262.utf8_text";
  let k = Units.first_invalid_utf8_range s off len in
  if k >= 0 then Error k else Ok { Units.s; base = off; len }

let utf8_text_exn ?off ?len s =
  match utf8_text ?off ?len s with
  | Ok t -> t
  | Error k -> invalid_arg (Printf.sprintf "Corvus_regex_ecma262: the text is not UTF-8 at byte %d" k)

(* What the operations over each encoding need of it. *)
module type ENC = sig
  val utf8 : bool
  val match_with : Prog.t -> Vm_state.t -> Units.text -> int -> bool -> int -> int
  val dfa_is_match : Dfa.t -> Units.text -> int
  val normalize : Units.text -> int -> bool -> int
  val advance : Units.text -> int -> bool -> int
  val push_lossy : Buffer.t -> Units.text -> int -> int -> unit
  val get : Units.text -> int -> int
end

module E8 = struct
  let utf8 = true
  let match_with = Vm8.match_with
  let dfa_is_match = Vm8.dfa_is_match
  let normalize = Units.Units8.normalize
  let advance = Units.Units8.advance
  let push_lossy = Units.Units8.push_lossy
  let get = Units.Units8.get
end

module E16 = struct
  let utf8 = false
  let match_with = Vm16.match_with
  let dfa_is_match = Vm16.dfa_is_match
  let normalize = Units.Units16.normalize
  let advance = Units.Units16.advance
  let push_lossy = Units.Units16.push_lossy
  let get = Units.Units16.get
end

(* A match: the text, and the start and end of the match and of each group (-1 for a group that did not take part). *)
type m = { text : Units.text; slots : int array; mnames : group_name list; utf8 : bool }

(* The slots of the last match, and the working memory of the matches into them: captures are used by one match at a
   time, so they keep a working memory of their own. *)
type captures = { mutable cslots : int array; cstate : Vm_state.t }

let captures re = { cslots = Array.make (2 * (re.group_count + 1)) (-1); cstate = Vm_state.create () }

module Ops (E : ENC) = struct
  (* The first match at or after start (at start alone with sticky). With all_groups, out receives the start and end
     of the match and of every group; without it, the start and end of the match. It returns whether there is one. *)
  (* exec with the working memory st. out may be st.out, which the match is through with when out is written. *)
  let exec_with re st (t : Units.text) start sticky all_groups (out : int array) =
    let start = E.normalize t start (unicode re) in
    let prog = if all_groups then caps_program re else re.prog in
    let from = E.match_with prog st t start sticky re.timeout in
    if from >= 0 then begin
      out.(0) <- from;
      out.(1) <- st.end_;
      if all_groups then Array.blit st.caps 0 out 2 (2 * prog.p.groups)
    end;
    from

  let result code = if code >= 0 then true else if code = Vm_state.no_match then false else raise_stopped code

  let exec re (t : Units.text) start sticky all_groups (out : int array) =
    let st = take_state re in
    let code = exec_with re st t start sticky all_groups out in
    release_state re st;
    result code

  (* The position a global search goes on from after a match from..to. *)
  let next_start re t from to_ = if to_ = from then E.advance t to_ (unicode re) else to_

  let piece (t : Units.text) from to_ =
    let b = Buffer.create (max 0 (to_ - from)) in
    E.push_lossy b t from to_;
    Buffer.contents b

  let is_match ?(pos = 0) re t =
    let decided =
      match re.dfa with
      | Some d when pos = 0 && not re.flags.sticky -> (
          match Once.get d with Some d -> E.dfa_is_match d t | None -> -1)
      | _ -> -1
    in
    if decided >= 0 then decided = 1
    else begin
      let st = take_state re in
      let code = exec_with re st t pos re.flags.sticky false st.out in
      release_state re st;
      result code
    end

  let find ?(pos = 0) re t =
    let slots = Array.make (2 * (re.group_count + 1)) (-1) in
    if exec re t pos re.flags.sticky true slots then Some { text = t; slots; mnames = re.names; utf8 = E.utf8 }
    else None

  let exec_into re caps ~pos t =
    let need = 2 * (re.group_count + 1) in
    if Array.length caps.cslots <> need then caps.cslots <- Array.make need (-1)
    else Array.fill caps.cslots 0 need (-1);
    result (exec_with re caps.cstate t pos re.flags.sticky true caps.cslots)

  let count re t =
    let st = take_state re in
    let n = ref 0 and at = ref 0 and code = ref 0 in
    while !code >= 0 do
      code := exec_with re st t !at re.flags.sticky false st.out;
      if !code >= 0 then begin
        incr n;
        at := next_start re t st.out.(0) st.out.(1)
      end
    done;
    release_state re st;
    ignore (result !code);
    !n

  let matches re t =
    let rec from at () =
      match find ~pos:at re t with
      | None -> Seq.Nil
      | Some m -> Seq.Cons (m, from (next_start re t m.slots.(0) m.slots.(1)))
    in
    from 0

  (* Whether the units of a part of the template are the name's, in this encoding. *)
  let name_is (tpl : Units.text) from to_ g =
    let name = if E.utf8 then g.name else g.name16 in
    let width = if E.utf8 then 1 else 2 in
    String.length name = (to_ - from) * width && String.sub tpl.s (tpl.base + (from * width)) ((to_ - from) * width) = name

  (* Appends the template with each $ form replaced (GetSubstitution). *)
  let substitute out (t : Units.text) (slots : int array) names (tpl : Units.text) =
    let unit k = E.get tpl k in
    let digit k = if k < tpl.len && unit k >= 0x30 && unit k <= 0x39 then unit k - 0x30 else -1 in
    let groups = (Array.length slots / 2) - 1 in
    let from = slots.(0) and to_ = slots.(1) in
    let group g = if slots.(2 * g) >= 0 then E.push_lossy out t slots.(2 * g) slots.((2 * g) + 1) in
    let push_unit k = E.push_lossy out tpl k (k + 1) in
    let k = ref 0 in
    while !k < tpl.len do
      let c = unit !k in
      if c <> Char.code '$' || !k + 1 >= tpl.len then begin
        push_unit !k;
        incr k
      end
      else begin
        let next = unit (!k + 1) in
        if next = Char.code '$' then begin
          push_unit !k;
          k := !k + 2
        end
        else if next = Char.code '&' then begin
          E.push_lossy out t from to_;
          k := !k + 2
        end
        else if next = Char.code '`' then begin
          E.push_lossy out t 0 from;
          k := !k + 2
        end
        else if next = Char.code '\'' then begin
          E.push_lossy out t to_ t.len;
          k := !k + 2
        end
        else if next >= 0x30 && next <= 0x39 then begin
          let d1 = next - 0x30 and d2 = digit (!k + 2) in
          if d2 >= 0 && (d1 * 10) + d2 >= 1 && (d1 * 10) + d2 <= groups then begin
            group ((d1 * 10) + d2);
            k := !k + 3
          end
          else if d1 >= 1 && d1 <= groups then begin
            group d1;
            k := !k + 2
          end
          else begin
            push_unit !k;
            incr k
          end
        end
        else if next = Char.code '<' then begin
          (* Without a named group $< is itself. With one, the name runs to the next >; a name that is no group's is the
             empty string, and a < with no > after it is itself. *)
          let close = ref (-1) and j = ref (!k + 2) in
          while !close < 0 && !j < tpl.len do
            if unit !j = Char.code '>' then close := !j else incr j
          done;
          if !close >= 0 && names <> [] then begin
            (match List.find_opt (name_is tpl (!k + 2) !close) names with
            | Some g -> (
                match List.find_opt (fun g -> slots.(2 * g) >= 0) (Array.to_list g.groups) with
                | Some g -> group g
                | None -> ())
            | None -> ());
            k := !close + 1
          end
          else begin
            push_unit !k;
            incr k
          end
        end
        else begin
          push_unit !k;
          incr k
        end
      end
    done

  (* Replaces the first match, or every match with all, by the template. *)
  let replace_gen re (t : Units.text) (tpl : Units.text) all =
    let slots = Array.make (2 * (re.group_count + 1)) (-1) in
    let out = Buffer.create (t.len + 16) in
    let last = ref 0 and at = ref 0 and any = ref false and fin = ref false in
    while not !fin do
      if exec re t !at re.flags.sticky true slots then begin
        any := true;
        let from = slots.(0) and to_ = slots.(1) in
        E.push_lossy out t !last from;
        substitute out t slots re.names tpl;
        last := to_;
        if not all then fin := true else at := next_start re t from to_
      end
      else fin := true
    done;
    if not !any then String.sub t.s (t.base * if E.utf8 then 1 else 2) (t.len * if E.utf8 then 1 else 2)
    else begin
      E.push_lossy out t !last t.len;
      Buffer.contents out
    end

  (* Splits the text around the matches (RegExp.prototype[Symbol.split]), with at most limit items. *)
  let split_gen re (t : Units.text) limit =
    let size = t.len in
    let out = ref [] and n = ref 0 in
    if limit = 0 then []
    else begin
      let slots = Array.make (2 * (re.group_count + 1)) (-1) in
      if size = 0 then begin
        if not (exec re t 0 true true slots) then [ Some (piece t 0 0) ] else []
      end
      else begin
        let p = ref 0 and q = ref 0 and fin = ref false in
        let add v =
          out := v :: !out;
          incr n;
          if !n = limit then fin := true
        in
        while (not !fin) && !q < size do
          if not (exec re t !q false true slots) then q := size
          else begin
            let from = slots.(0) and to_ = slots.(1) in
            if from >= size then q := size
            else begin
              let e = min to_ size in
              if e = !p then q := E.advance t from (unicode re)
              else begin
                add (Some (piece t !p from));
                p := e;
                let g = ref 1 in
                while (not !fin) && !g < Array.length slots / 2 do
                  add (if slots.(2 * !g) >= 0 then Some (piece t slots.(2 * !g) slots.((2 * !g) + 1)) else None);
                  incr g
                done;
                q := !p
              end
            end
          end
        done;
        if not !fin then add (Some (piece t !p size));
        List.rev !out
      end
    end
end

module O8 = Ops (E8)
module O16 = Ops (E16)

let is_match ?pos re s = O8.is_match ?pos re (text8 re s)
let is_match_text ?pos re t = O8.is_match ?pos re t
let is_match_utf16 ?pos re s = O16.is_match ?pos re (text16 re s)
let find ?pos re s = O8.find ?pos re (text8 re s)
let find_text ?pos re t = O8.find ?pos re t
let find_utf16 ?pos re s = O16.find ?pos re (text16 re s)
let exec_into re caps ~pos s = O8.exec_into re caps ~pos (text8 re s)
let exec_into_text re caps ~pos t = O8.exec_into re caps ~pos t
let exec_into_utf16 re caps ~pos s = O16.exec_into re caps ~pos (text16 re s)
let count re s = O8.count re (text8 re s)
let count_text re t = O8.count re t
let count_utf16 re s = O16.count re (text16 re s)
let matches re s = O8.matches re (text8 re s)
let matches_utf16 re s = O16.matches re (text16 re s)
let replace re s tpl = O8.replace_gen re (text8 re s) (utf8_text_of tpl) re.flags.global
let replace_all re s tpl = O8.replace_gen re (text8 re s) (utf8_text_of tpl) true
let replace_utf16 re s tpl = O16.replace_gen re (text16 re s) (utf16_text_of tpl) re.flags.global
let replace_all_utf16 re s tpl = O16.replace_gen re (text16 re s) (utf16_text_of tpl) true
let split ?(limit = -1) re s = O8.split_gen re (text8 re s) (if limit < 0 then max_int else limit)
let split_utf16 ?(limit = -1) re s = O16.split_gen re (text16 re s) (if limit < 0 then max_int else limit)

let capture_start caps i = if 2 * i < Array.length caps.cslots then caps.cslots.(2 * i) else -1
let capture_end caps i = if (2 * i) + 1 < Array.length caps.cslots then caps.cslots.((2 * i) + 1) else -1

module Match = struct
  type t = m

  let start m = m.slots.(0)
  let end_ m = m.slots.(1)
  let group_count m = (Array.length m.slots / 2) - 1

  let span m i =
    if i < 0 || (2 * i) + 1 >= Array.length m.slots || m.slots.(2 * i) < 0 then None
    else Some (m.slots.(2 * i), m.slots.((2 * i) + 1))

  let piece m from to_ =
    let b = Buffer.create (max 0 (to_ - from)) in
    (if m.utf8 then Units.Units8.push_lossy else Units.Units16.push_lossy) b m.text from to_;
    Buffer.contents b

  let group m i = match span m i with None -> None | Some (f, e) -> Some (piece m f e)
  let value m = piece m m.slots.(0) m.slots.(1)

  let named m name =
    match List.find_opt (fun g -> g.name = name) m.mnames with
    | None -> None
    | Some g -> (
        match List.find_opt (fun g -> m.slots.(2 * g) >= 0) (Array.to_list g.groups) with
        | Some g -> group m g
        | None -> None)
end
