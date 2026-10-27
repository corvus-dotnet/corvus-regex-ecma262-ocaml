(* Finding where a match can start (section 4.3 of the design of the .NET reference engine), worked out once from the
   syntax tree when a pattern is compiled (search.rs of the Rust crate).

   A search on a long text spends most of its time looking for where to start, so before the matcher runs the program
   applies what it learned at compile time. What is searched for (an "anchor") is a literal, each of whose characters
   may have several forms (a letter's cases under the i flag, or the members of a small class), that every match holds
   at a known number of characters from its start: one number, or a bounded range. A pattern that is an alternation, or
   that holds one after a bounded prefix (.{0,2}(Tom|Sawyer)), has an anchor in each alternative, and all of them are
   searched for at once (the Literals module). Each occurrence opens the window of starts from which some alternative
   could reach it; the starts are tried in order and never twice (the alternation windows). Where every alternative
   starts with its literal, the matcher runs at the occurrence alone.

   A pattern that starts with loops with no upper bound, then a literal ([a-zA-Z]+ing, \w+\s+Holmes), has the literal
   searched for and the runs of the loops' sets before it walked back to the earliest start, which is the only one tried
   for that occurrence: a later start in the same run can only reach a subset of the places the loops can end at. *)

open Parse

(* The most literals the forms of one anchor are expanded to. *)
let max_forms = 64

(* The most literals of all the anchors of a pattern. *)
let max_literals = 512

(* The most characters a window of starts may span. *)
let max_window = 64

(* The checks a rest check makes before it decides whether it pays. *)
let rest_trials = 64

(* A bound past every window, so that sums of widths saturate there. *)
let far = 1 lsl 40

(* The anchored automaton of what follows an occurrence (an anchor and the terms after it), run at the occurrence before
   any start is walked back to. A check that seldom rules an occurrence out costs more than it saves (an e-mail
   address's @ is nearly always followed by a domain), so the check keeps count over its first trials and is dropped for
   good when fewer than one in eight of them ruled the occurrence out. The counts change only how fast a search is,
   never what it finds. *)
type rest_check = { dfa : Adfa.anchored; checks : int Atomic.t; rejects : int Atomic.t }

let rest_check dfa = { dfa; checks = Atomic.make 0; rejects = Atomic.make 0 }

(* Whether the check is still made. *)
let[@inline] pays r = Atomic.get r.checks < rest_trials || Atomic.get r.rejects * 8 >= rest_trials

(* Counts a check while it is still on trial: each of the first trials once, whichever domains make them, and none
   after. *)
let count r out =
  if Atomic.get r.checks < rest_trials && Atomic.fetch_and_add r.checks 1 < rest_trials && out then
    Atomic.incr r.rejects

(* How the end of a run of a loop's members is found (RunEnd of search.rs). Each finds it a member at a time. All: the
   set holds every character, and the run is the rest of the text. Stops: the set holds every character but up to three
   ASCII ones (their number, then the bytes with the last repeated to three), found by the searches for bytes, with the units that end a run over UTF-16.
   Line: the set holds every character but the four line terminators (the dot): LF and CR are found by the search for
   bytes, with the lead byte of U+2028 and U+2029, which is looked at further where it is found. *)
type run_end = Each | All | Stops of int * int * int * int * Literals.unit_set | Line of Literals.unit_set

(* The shape of a pattern whose matches the search finds without running the matcher (the .NET reference engine's
   simple shapes). Only for a pattern with no group. Literals_only is literals and nothing else, an alternation of them
   or one, with every form of each written out: the search's own match, which is the first literal in the pattern's
   order of those that start leftmost. Forms is an alternation of literals each of whose characters has several forms:
   at each occurrence the search finds, the alternatives are compared in order. Set_shape is one class and nothing
   else: every member is a match. Loop is one greedy loop of a class with no upper bound and a least count of zero or
   one: a match is the run of members from where it starts. *)
type shape =
  | No_shape
  | Literals_only
  | Forms of int array array array
  | Set_shape of Charset.t
  | Loop of Charset.t * int * run_end

(* How the search finds where a match can start, beyond the first set. Scan tries every position the first set allows.
   Windows: every match holds one of the literals lo to hi characters after its start; rest is the anchored automaton
   of the anchors and the terms after each, run at an occurrence first where a window has more than one start: where
   nothing from the occurrence on can match, its window is not tried. After_loops: every match is runs of the loops'
   sets, then the literal; rest is the anchored automaton of the literal and the terms after it, run at each occurrence
   first. *)
type plan =
  | Scan
  | Windows of { literals : Literals.t; lo : int; hi : int; reverse : Adfa.reverse option; rest : rest_check option }
  | After_loops of { literals : Literals.t; sets : Charset.t array; mins : int array; rest : rest_check option }

(* A search for any of a few bytes: the searches for one to four bytes, a compare for a run of ASCII bytes, the search
   for up to four ranges of bytes, or a table. *)
type first_bytes = {
  fcount : int;
  a : int;
  b : int;
  c : int;
  d : int;
  (* The bytes are the ASCII ones from lo to hi, which one compare finds (the digits, the letters). *)
  is_range : bool;
  lo : int;
  hi : int;
  (* Every byte, as a set. *)
  table : string;
  (* Up to four ranges that hold every byte, and maybe more: the runs of the ASCII bytes, and one run from the least to
     the greatest lead byte of the others (every byte between is a lead byte too, so a candidate is never inside a
     character). A candidate is tested against the first set, so a byte too many costs a test and nothing else. ranges
     are as Kernels.index_ranges reads them (the one range lo to hi where is_range); nranges is 0 when there are more. *)
  nranges : int;
  ranges : string;
}

(* What a pattern's search knows of it. Nothing changes it once it is built, but the counts of its rest check. *)
type t = {
  plan : plan;
  shape : shape;
  (* The set of a loop every match starts with, when a start that fails may move to the end of the run of its set:
     every start in the rest of the run can only reach a subset of the places the loop can end at. *)
  skip_run : Charset.t option;
  (* The pattern starts with \b and every character a match can start with is an ASCII word character, so a start
     inside a word fails and the search passes over the rest of the word. *)
  leading_word_boundary : bool;
  (* The bytes a match can start with over UTF-8, searched for together when they are few: the first set's ASCII
     members and the lead bytes of its other members. *)
  first_bytes : first_bytes option;
  (* The code units a match can start with over UTF-16. *)
  first_units : Literals.unit_set option;
}

let none =
  { plan = Scan; shape = No_shape; skip_run = None; leading_word_boundary = false; first_bytes = None; first_units = None }

(* Makes every pattern compiled while it is set try every position its first set allows, which the tests compare the
   searches with. *)
let no_search = ref false

(* One literal every match of an alternative holds, lo to hi characters after its start: the forms of each of its
   characters, the terms a match goes through before the anchor, and the terms from the anchor on, to the end of the
   pattern. *)
type anchor = { chars : int array array; alo : int; ahi : int; prefix : node array; rest : node array }

let add_far a b = min (a + b) far

let mul_far a b =
  if a = 0 || b = 0 then 0
  else
    let a = min a far and b = min b far in
    if a >= far / b then far else a * b

(* The search for the bytes a set's members start with, or None when they are none or too many. *)
let first_bytes_of (s : Charset.t) units =
  let table = Bytes.make 256 '\000' in
  let spans = [| (0, 0x7F, 0, 0x00); (0x80, 0x7FF, 6, 0xC0); (0x800, 0xFFFF, 12, 0xE0); (0x10000, 0x10FFFF, 18, 0xF0) |] in
  try
    let r = s.ranges in
    let k = ref 0 in
    while !k < Array.length r do
      let lo = r.(!k) and hi = r.(!k + 1) in
      (* A surrogate without the u flag is half of a character of four bytes, and U+FFFD is also what a byte that is
         not UTF-8 reads as: neither starts at a byte the set can name. *)
      if (units && lo <= 0xDFFF && hi >= 0xD800) || (lo <= 0xFFFD && hi >= 0xFFFD) then raise Exit;
      Array.iter
        (fun (from, to_, shift, lead) ->
          let x = max lo from and y = min hi to_ in
          if x <= y then
            for b = lead lor (x lsr shift) to lead lor (y lsr shift) do
              Bytes.set table b '\001'
            done)
        spans;
      k := !k + 2
    done;
    let list = List.filter (fun b -> Bytes.get table b <> '\000') (List.init 256 Fun.id) |> Array.of_list in
    let n = Array.length list in
    if n = 0 || n > max_forms then None
    else begin
      let at k = list.(min k (n - 1)) in
      let lo = list.(0) and hi = list.(n - 1) in
      let is_range = n > 4 && hi < 0x80 && hi - lo = n - 1 in
      let in_table b = Bytes.get table b <> '\000' in
      let nranges, ranges =
        if n > 4 && not is_range then begin
          (* The runs of the ASCII bytes, then the lead bytes as one run. *)
          let pairs = Buffer.create 8 in
          let b = ref 0 in
          while !b < 0x80 do
            if in_table !b then begin
              let x = !b in
              while !b + 1 < 0x80 && in_table (!b + 1) do
                incr b
              done;
              Buffer.add_char pairs (Char.chr x);
              Buffer.add_char pairs (Char.chr (!b - x))
            end;
            incr b
          done;
          if hi >= 0x80 then begin
            let x = ref 0x80 in
            while not (in_table !x) do
              incr x
            done;
            Buffer.add_char pairs (Char.chr !x);
            Buffer.add_char pairs (Char.chr (hi - !x))
          end;
          if Buffer.length pairs <= 8 then (Buffer.length pairs / 2, Buffer.contents pairs) else (0, "")
        end
        else if is_range then (1, String.init 2 (fun k -> Char.chr (if k = 0 then lo else hi - lo)))
        else (0, "")
      in
      Some
        {
          fcount = n;
          a = at 0;
          b = at 1;
          c = at 2;
          d = at 3;
          is_range;
          lo;
          hi;
          table = Bytes.to_string table;
          nranges;
          ranges;
        }
    end
  with Exit -> None

(* The test for the code units a match can start with over UTF-16: the first set's units as up to eight ranges (a
   member beyond the BMP as its lead surrogate), where more are merged across the narrowest gaps, the ASCII and Latin
   ones last since those units are common in text. A candidate is then tested against the first set itself. *)
let first_units_of (s : Charset.t) =
  let lead cp = 0xD800 + ((cp - 0x10000) lsr 10) in
  let units = ref [] in
  let r = s.ranges in
  let k = ref 0 in
  while !k < Array.length r do
    let lo = r.(!k) and hi = r.(!k + 1) in
    if lo <= 0xFFFF then units := (lo, min hi 0xFFFF) :: !units;
    if hi > 0xFFFF then units := (lead (max lo 0x10000), lead hi) :: !units;
    k := !k + 2
  done;
  let units = List.sort compare !units in
  let merged =
    List.fold_left
      (fun acc (lo, hi) ->
        match acc with (plo, phi) :: rest when lo <= phi + 1 -> (plo, max phi hi) :: rest | _ -> (lo, hi) :: acc)
      [] units
    |> List.rev |> Array.of_list
  in
  let merged = ref merged in
  while Array.length !merged > 8 do
    (* The gap whose units are least likely in text: its width, a thousand times over below U+0300. *)
    let m = !merged in
    let cost k =
      let a = snd m.(k) + 1 and b = fst m.(k + 1) in
      (b - a) * if a < 0x300 then 1000 else 1
    in
    let best = ref 0 in
    for k = 1 to Array.length m - 2 do
      if cost k < cost !best then best := k
    done;
    merged :=
      Array.init
        (Array.length m - 1)
        (fun k -> if k < !best then m.(k) else if k = !best then (fst m.(k), snd m.(k + 1)) else m.(k + 1))
  done;
  let m = !merged in
  if Array.length m = 0 then None
  else Some (Literals.unit_set (Array.init (2 * Array.length m) (fun k -> if k mod 2 = 0 then fst m.(k / 2) else snd m.(k / 2))))

(* The forms of a character the search can look for: a literal code point, or the members of a small class. *)
let forms_of n =
  match n.kind with
  | Char -> if Compile.literal_rune n.r then Some [| n.r |] else None
  | Set -> (
      let r = (class_of n).ranges in
      let out = ref [] and count = ref 0 in
      try
        let k = ref 0 in
        while !k < Array.length r do
          let lo = r.(!k) and hi = r.(!k + 1) in
          if hi - lo >= 4 || !count + (hi - lo + 1) > 4 then raise Exit;
          for c = lo to hi do
            if not (Compile.literal_rune c) then raise Exit;
            out := c :: !out;
            incr count
          done;
          k := !k + 2
        done;
        (* An empty class has no form to look for. *)
        if !count > 0 then Some (Array.of_list (List.rev !out)) else None
      with Exit -> None)
  | _ -> None

let has_forms n = forms_of n <> None

(* The terms of a node in order: a sequence and a group whose body is a sequence are spliced in. *)
let terms n =
  let out = ref [] in
  let rec add n =
    match n.kind with
    | Cat -> Array.iter add n.subs
    | Group -> if n.subs.(0).kind <> Alt then add n.subs.(0) else out := n :: !out
    | Empty -> ()
    | _ -> out := n :: !out
  in
  add n;
  Array.of_list (List.rev !out)

(* The least and the greatest number of characters a node consumes (the greatest -1 for no bound), or None when it is
   not known (a backreference). *)
let rec width n =
  match n.kind with
  | Char | Set -> Some (1, 1)
  | Cat ->
      Array.fold_left
        (fun acc sub ->
          match (acc, width sub) with
          | Some (lo, hi), Some (a, b) -> Some (add_far lo a, if hi < 0 || b < 0 then -1 else add_far hi b)
          | _ -> None)
        (Some (0, 0)) n.subs
  | Alt ->
      let first = ref true in
      Array.fold_left
        (fun acc sub ->
          match (acc, width sub) with
          | Some (lo, hi), Some (a, b) ->
              if !first then begin
                first := false;
                Some (a, b)
              end
              else Some (min lo a, if hi < 0 || b < 0 then -1 else max hi b)
          | _ -> None)
        (Some (0, 0)) n.subs
  | Group -> width n.subs.(0)
  | Repeat -> (
      match width n.subs.(0) with
      | None -> None
      | Some (a, b) ->
          let lo = mul_far a n.min in
          let hi = if n.max = unbounded && b = 0 then 0 else if n.max = unbounded || b < 0 then -1 else mul_far b n.max in
          Some (lo, hi))
  | Backref -> None
  | _ -> Some (0, 0)

let slice (l : node array) from = Array.sub l from (Array.length l - from)

(* The anchors of n, which the terms before precede and the terms after follow, whose match starts lo to hi characters
   after the start of the whole match: the first run of literal characters of each of its alternatives. None when some
   alternative has none at a bounded distance. *)
let rec anchors_after n lo hi (before : node array) (after : node array) =
  let ts = terms n in
  let prefix = ref (Array.to_list before |> List.rev) in
  let rec go k lo hi =
    if k >= Array.length ts then None
    else begin
      let t = ts.(k) in
      if has_forms t then begin
        let rest = Array.append (slice ts k) after in
        let chars = ref [] and k = ref k in
        let continue = ref true in
        while !continue && !k < Array.length ts do
          match forms_of ts.(!k) with
          | Some f ->
              chars := f :: !chars;
              incr k
          | None -> continue := false
        done;
        Some [ { chars = Array.of_list (List.rev !chars); alo = lo; ahi = hi; prefix = Array.of_list (List.rev !prefix); rest } ]
      end
      else begin
        let alt = if t.kind = Alt then Some t else if t.kind = Group && t.subs.(0).kind = Alt then Some t.subs.(0) else None in
        match alt with
        | Some alt -> (
            let following = Array.append (slice ts (k + 1)) after in
            let pre = Array.of_list (List.rev !prefix) in
            try
              Some
                (List.concat_map
                   (fun sub -> match anchors_after sub lo hi pre following with Some l -> l | None -> raise Exit)
                   (Array.to_list alt.subs))
            with Exit -> None)
        | None -> (
            match width t with
            | Some (a, b) when b >= 0 ->
                let lo = add_far lo a and hi = add_far hi b in
                if hi > max_window then None
                else begin
                  prefix := t :: !prefix;
                  go (k + 1) lo hi
                end
            | _ -> None)
      end
    end
  in
  go 0 lo hi

(* The literals of an anchor: the forms of its first count characters written out. *)
let expand a count =
  let result = ref [ [] ] in
  for c = 0 to count - 1 do
    result := List.concat_map (fun p -> List.map (fun r -> r :: p) (Array.to_list a.chars.(c))) !result
  done;
  List.map (fun l -> Array.of_list (List.rev l)) !result

(* The most bytes of each of an anchor's literals written out over UTF-8: where the anchor was cut short, every literal
   is cut to the bytes of its shortest, so that the forms of a character longer than the others (the long s, two bytes
   beside the one of s) make no more literals than the others. The regex crate's rule. *)
let utf8_cuts cut_short (lits : int array list) =
  let cut =
    if cut_short then
      List.fold_left (fun m l -> min m (Array.fold_left (fun s r -> s + Compile.rune_len r) 0 l)) max_int lits
    else max_int
  in
  List.map (fun _ -> cut) lits

(* The number of literals the anchors make with the first k characters of each written out. *)
let total_literals (anchors : anchor list) k =
  List.fold_left
    (fun total a ->
      let p = ref 1 in
      for c = 0 to min k (Array.length a.chars) - 1 do
        p := mul_far !p (Array.length a.chars.(c))
      done;
      add_far total !p)
    0 anchors

(* How many of each anchor's characters to write out, and whether the anchors were cut short. Exact anchors are written
   out in full. Where a character has several forms (a letter under the i flag), every anchor is cut to one length by
   the rule of the regex crate's literal optimisation (Seq::optimize_by_preference): the full literals if they are ten
   or fewer, else five characters if that makes ten or fewer, else four if that makes sixty-four or fewer, else three
   if sixty-four or fewer, else two if ten or fewer, else one. *)
let literal_counts (anchors : anchor list) =
  let all = max_int in
  let exact = List.for_all (fun a -> Array.for_all (fun f -> Array.length f = 1) a.chars) anchors in
  let k =
    if exact || total_literals anchors all <= 10 then all
    else if total_literals anchors 5 <= 10 then 5
    else if total_literals anchors 4 <= max_forms then 4
    else if total_literals anchors 3 <= max_forms then 3
    else if total_literals anchors 2 <= 10 then 2
    else 1
  in
  (List.map (fun a -> min k (Array.length a.chars)) anchors, (not exact) && k <> all)

(* Whether a set holds ASCII word characters alone. *)
let ascii_word_set (f : Charset.t option) =
  match f with
  | None -> false
  | Some f ->
      (not (Charset.is_empty f))
      && f.ranges.(Array.length f.ranges - 1) < 0x80
      &&
      let ok = ref true in
      for c = 0 to 0x7F do
        if Charset.has_ascii f c && not (Charset.is_word_byte c) then ok := false
      done;
      !ok

(* The top-level alternatives of a tree: the alternatives of an alternation (in a group or not), or the tree. *)
let alternatives root =
  if root.kind = Alt then root.subs
  else if root.kind = Group && root.group = 0 && root.subs.(0).kind = Alt then root.subs.(0).subs
  else [| root |]

let rec holds_group n = (n.kind = Group && n.group > 0) || Array.exists holds_group n.subs

(* The literal shapes of a tree whose anchors are found: literals and nothing else, exact or with forms. *)
let shape_of_literals root (found : anchor list) counts cut_short =
  if holds_group root then No_shape
  else
    try
      let all =
        Array.map
          (fun alt ->
            let ts = terms alt in
            if Array.length ts = 0 then raise Exit;
            Array.map (fun t -> match forms_of t with Some f -> f | None -> raise Exit) ts)
          (alternatives root)
      in
      (* Literals written out in full, every form of each, are the search's own literals, in the pattern's order (an
         alternative's forms together), so the search's leftmost-first match is the pattern's. *)
      let literal =
        (not cut_short)
        && List.length found = Array.length all
        && List.for_all2 (fun (a, c) forms -> a.alo = 0 && a.ahi = 0 && c = Array.length forms) (List.combine found counts)
             (Array.to_list all)
      in
      if literal then Literals_only else Forms all
    with Exit -> No_shape

(* The set of a loop of one character or class with no upper bound, or None for any other term. *)
let open_loop t =
  if t.kind <> Repeat || t.max <> unbounded then None
  else
    let body = t.subs.(0) in
    match body.kind with
    | Set -> Some (class_of body)
    | Char -> Some (Charset.make [| body.r; body.r |])
    | _ -> None

(* How the end of a run of the set's members is found. *)
let run_end_of (cs : Charset.t) =
  let c = (Charset.complement cs).ranges in
  if Array.length c = 0 then All
  else if c = Charset.line_terminator_pairs then Line (Literals.unit_set (Array.copy c))
  else
    try
      let stops = ref [] in
      let k = ref 0 in
      while !k < Array.length c do
        let lo = c.(!k) and hi = c.(!k + 1) in
        if hi >= 0x80 || hi - lo >= 3 || List.length !stops + (hi - lo + 1) > 3 then raise Exit;
        for x = lo to hi do
          stops := !stops @ [ x ]
        done;
        k := !k + 2
      done;
      let s = Array.of_list !stops in
      let at k = s.(min k (Array.length s - 1)) in
      Stops (Array.length s, at 0, at 1, at 2, Literals.unit_set (Array.copy c))
    with Exit -> Each

(* The class and loop shapes of a tree that is one term. *)
let shape_of_one root =
  if holds_group root then No_shape
  else
    let ts = terms root in
    if Array.length ts <> 1 then No_shape
    else
      let t = ts.(0) in
      if t.kind = Set && not (Charset.is_empty (class_of t)) then Set_shape (class_of t)
      else if t.kind = Repeat && (not t.lazy_) && t.min <= 1 then
        match open_loop t with
        | Some cs when not (Charset.is_empty cs) -> Loop (cs, t.min, run_end_of cs)
        | _ -> No_shape
      else No_shape

let rest_of dfa = match dfa with Some d -> Some (rest_check d) | None -> None

(* The search of a tree, after simplify. backrefs says the pattern has a backreference, which rules out what depends on
   a start reaching a subset of another's places; first is the first set of the program, when there is one. *)
let plan root backrefs (first : Charset.t option) units =
  if !no_search then none
  else begin
    let first_bytes, first_units =
      match first with Some f -> (first_bytes_of f units, first_units_of f) | None -> (None, None)
    in
    let ts = terms root in
    (* A leading \b before characters that are all ASCII word characters. *)
    let boundary = Array.length ts > 0 && ts.(0).kind = Word_boundary && not ts.(0).fold in
    let base =
      { none with first_bytes; first_units; leading_word_boundary = boundary && ascii_word_set first }
    in
    let windows =
      match anchors_after root 0 0 [||] [||] with
      | None -> None
      | Some found ->
          let counts, cut_short = literal_counts found in
          let lits = ref [] and cuts = ref [] in
          let lo = ref max_int and hi = ref 0 in
          List.iter2
            (fun a count ->
              lo := min !lo a.alo;
              hi := max !hi a.ahi;
              let expanded = expand a count in
              let c = utf8_cuts cut_short expanded in
              List.iter2
                (fun e c ->
                  if not (List.mem e !lits) then begin
                    lits := e :: !lits;
                    cuts := c :: !cuts
                  end)
                expanded c)
            found counts;
          let lits = List.rev !lits and cuts = List.rev !cuts in
          let n = List.length lits in
          if n = 0 || n > max_literals || List.exists (fun l -> Array.length l = 0) lits then None
          else begin
            let shape = shape_of_literals root found counts cut_short in
            let literals = Literals.make (Array.of_list lits) (Array.of_list cuts) in
            (* Where a window has more than one start, the terms before the anchors are matched backwards from each
               occurrence, so that only the starts they allow are tried (the reverse search of the regex crate). *)
            let reverse, rest =
              if !hi > 0 then begin
                let reverse = Adfa.reverse (List.map (fun a -> a.prefix) found) in
                (* Where every anchor is the end of its alternative, the occurrence is all the rest there is to check. *)
                let longer = List.exists (fun a -> Array.length a.rest > Array.length a.chars) found in
                let rest =
                  if longer then
                    rest_of
                      (Adfa.build (with_subs Alt (Array.of_list (List.map (fun a -> with_subs Cat a.rest) found))))
                  else None
                in
                (reverse, rest)
              end
              else (None, None)
            in
            Some { base with shape; plan = Windows { literals; lo = !lo; hi = !hi; reverse; rest } }
          end
    in
    match windows with
    | Some s -> s
    | None ->
        let base = { base with shape = shape_of_one root } in
        if backrefs then base
        else begin
          (* Loops with no upper bound, then a literal. *)
          let k = ref (if boundary then 1 else 0) in
          let sets = ref [] and mins = ref [] in
          let continue = ref true in
          while !continue && !k < Array.length ts do
            match open_loop ts.(!k) with
            | Some cs ->
                sets := cs :: !sets;
                mins := ts.(!k).min :: !mins;
                incr k
            | None -> continue := false
          done;
          let sets = Array.of_list (List.rev !sets) and mins = Array.of_list (List.rev !mins) in
          if Array.length sets = 0 then base
          else begin
            (* \b is allowed when every loop's set is of word characters (a later start of the runs can be in any loop's
               run, where \b holds after a word character before a member that is not one: \b\d*\s*a over " a"), and
               the sets must share no character, any two of them. *)
            let words = Array.for_all (fun s -> ascii_word_set (Some s)) sets in
            let disjoint = ref true in
            Array.iteri
              (fun i a ->
                for j = i + 1 to Array.length sets - 1 do
                  if not (Charset.is_empty (Charset.intersect a sets.(j))) then disjoint := false
                done)
              sets;
            let small = Array.for_all (fun m -> m <= 1) mins in
            let literal_at = !k in
            let chars = ref [] and continue = ref true in
            while !continue && !k < Array.length ts do
              match forms_of ts.(!k) with
              | Some f ->
                  chars := f :: !chars;
                  incr k
              | None -> continue := false
            done;
            let chars = Array.of_list (List.rev !chars) in
            let base =
              if ((not boundary) || words) && !disjoint && small && Array.length chars > 0 then begin
                let a = { chars; alo = 0; ahi = 0; prefix = [||]; rest = [||] } in
                let counts, cut_short = literal_counts [ a ] in
                let lits = expand a (List.hd counts) in
                let rest = slice ts literal_at in
                let rest =
                  if Array.length rest > Array.length chars then rest_of (Adfa.build (with_subs Cat rest)) else None
                in
                let literals = Literals.make (Array.of_list lits) (Array.of_list (utf8_cuts cut_short lits)) in
                { base with plan = After_loops { literals; sets; mins; rest } }
              end
              else base
            in
            (* A failed start may skip the rest of its leading loop's run (the loop is the first term). *)
            if boundary then base else { base with skip_run = Some sets.(0) }
          end
        end
  end
