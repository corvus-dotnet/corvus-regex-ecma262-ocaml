(* A deterministic automaton over ASCII text, for the patterns that need no backtracking (dfa.rs of the Rust crate).

   The strings a schema tests are short and nearly always ASCII, and most patterns in schemas are small. For such a
   pattern every state set the text can lead to is worked out once, and a match is then one table read per unit.

   The automaton reads units below 0x80 only. A text with any other unit is handed back to the backtracking matcher
   from the start, so nothing here needs to know UTF-8 or the Unicode classes: a class is its ASCII members. It takes
   \b and \B (not those of a case-insensitive pattern with the u flag, which fold): a state carries whether the unit
   before it was a word character and whether it is at the start of the text, and a boundary still to pass is passed,
   or not, when the next unit is known (section 4.4 of the design of the .NET reference engine). A pattern whose
   automaton has more states than a byte can number has no automaton. The automaton is built on the first match, not
   when the pattern is compiled, so a program pays nothing for the patterns it never matches. *)

open Parse

(* The states every automaton has. dead: no match can follow. accept: a match was found. bail: the text is not ASCII. *)
let dead = 0
let accept = 1
let bail = 2
let first_state = 3

(* An automaton has at most this many states, built from at most this many states of the pattern, in at most this many
   steps (a step is one state of the pattern looked at while a set is made). The steps bound what a pattern too large
   for an automaton costs before that is known, to about a millisecond. *)
let max_dfa_states = 256
let max_nfa_states = 512
let max_dfa_steps = 400_000

type nkind = NSet | NSplit | NBol | NEol | NWord | NNot_word | NMatch
type nstate = { nk : nkind; nset : string; mutable out : int; alt : int }

let no_set = String.make 128 '\000'
let nstate nk out = { nk; nset = no_set; out; alt = 0 }

type builder = { mutable states : nstate list; mutable count : int; mutable failed : bool }

let add b s =
  if b.count >= max_nfa_states then begin
    b.failed <- true;
    0
  end
  else begin
    b.states <- s :: b.states;
    b.count <- b.count + 1;
    b.count - 1
  end

(* The states of a node that go on to next; the state they start at. The states are numbered as they are added, so a
   loop's split, whose out is known only after its body, is patched through the array once it is built. *)
let compile root match_state =
  let b = { states = []; count = 0; failed = false } in
  let patches = ref [] in
  let m = add b match_state in
  let rec go n next =
    if b.failed then 0
    else
      match n.kind with
      | Empty -> next
      | Char ->
          let set = if n.r >= 0 && n.r < 128 then String.init 128 (fun c -> if c = n.r then '\001' else '\000') else no_set in
          add b { (nstate NSet next) with nset = set }
      | Set -> add b { (nstate NSet next) with nset = (class_of n).ascii }
      | Cat ->
          let next = ref next in
          for k = Array.length n.subs - 1 downto 0 do
            next := go n.subs.(k) !next
          done;
          !next
      | Alt ->
          let last = Array.length n.subs - 1 in
          let start = ref (go n.subs.(last) next) in
          for k = last - 1 downto 0 do
            let out = go n.subs.(k) next in
            start := add b { (nstate NSplit out) with alt = !start }
          done;
          !start
      | Group -> go n.subs.(0) next
      | Repeat ->
          let sub = n.subs.(0) in
          let next = ref next in
          if n.max = unbounded then begin
            (* The loop, then the copies that must match before it. *)
            let loop_ = add b { (nstate NSplit 0) with alt = !next } in
            if not b.failed then begin
              let out = go sub loop_ in
              patches := (loop_, out) :: !patches;
              next := loop_
            end
          end
          else begin
            (* The copies that may match, innermost last. *)
            let i = ref n.min in
            while !i < n.max && not b.failed do
              let out = go sub !next in
              next := add b { (nstate NSplit out) with alt = !next };
              incr i
            done
          end;
          let i = ref 0 in
          while !i < n.min && not b.failed do
            next := go sub !next;
            incr i
          done;
          !next
      | Bol -> add b (nstate NBol next)
      | Eol -> add b (nstate NEol next)
      | Word_boundary when not n.fold -> add b (nstate NWord next)
      | Not_word_boundary when not n.fold -> add b (nstate NNot_word next)
      (* A boundary that folds, or anything that needs the backtracking matcher. *)
      | _ ->
          b.failed <- true;
          0
  in
  let start = go root m in
  if b.failed then None
  else begin
    let nfa = Array.of_list (List.rev b.states) in
    List.iter (fun (i, out) -> nfa.(i).out <- out) !patches;
    Some (nfa, start)
  end

(* The automaton. A unit below 0x80 is one of a few classes (units the pattern does not tell apart; class 0 is any
   other), and trans.[state lsl shift lor class] is the state after it. *)
type t = {
  class_ : string;
  trans : string;
  shift : int;
  start : int;
  (* For each state: the pattern matches if the text ends there. *)
  at_end : bool array;
  (* The pattern matches the empty text. *)
  empty : bool;
}

exception Too_large

(* The automaton of a pattern for an unanchored search, or None when it has none. *)
let build root =
  match compile root (nstate NMatch 0) with
  | None -> None
  | Some (nfa, start) -> (
      let n = Array.length nfa in
      let mark = Array.make n 0 and generation = ref 0 and steps = ref max_dfa_steps in
      (* Every state that reading nothing reaches from the states in from, keeping the states that read a unit, the
         anchors and boundaries still to pass, and the match. A start anchor is passed only at_start, and an end anchor
         only at_end. A word boundary is kept to pass later without words, and with it (whether the units before and
         after the position are word characters) is passed or dropped. *)
      let closure from at_start at_end words =
        incr generation;
        let set = ref [] and stack = ref from in
        while !stack <> [] do
          let i = List.hd !stack in
          stack := List.tl !stack;
          decr steps;
          if mark.(i) <> !generation then begin
            mark.(i) <- !generation;
            let s = nfa.(i) in
            match s.nk with
            | NSplit -> stack := s.alt :: s.out :: !stack
            | NBol -> if at_start then stack := s.out :: !stack
            | NEol -> if at_end then stack := s.out :: !stack else set := i :: !set
            | NWord | NNot_word -> (
                match words with
                | None -> set := i :: !set
                | Some (before, after) -> if before <> after = (s.nk = NWord) then stack := s.out :: !stack)
            | _ -> set := i :: !set
          end
        done;
        List.sort_uniq compare !set
      in
      let has_match set = List.exists (fun i -> nfa.(i).nk = NMatch) set in
      let is_word c = Charset.is_word_byte c in
      (* The classes of units: two units are in one class when every set of the pattern has both or neither, and both or
         neither is a word character. Class 0 is the units that are not ASCII. *)
      let class_ = Bytes.make 129 '\000' and signatures = Hashtbl.create 64 and classes = ref 1 in
      for c = 0 to 127 do
        let sig_ = Buffer.create 64 in
        Buffer.add_char sig_ (if is_word c then '1' else '0');
        Array.iter (fun s -> if s.nk = NSet then Buffer.add_char sig_ s.nset.[c]) nfa;
        let key = Buffer.contents sig_ in
        let cl =
          match Hashtbl.find_opt signatures key with
          | Some cl -> cl
          | None ->
              Hashtbl.replace signatures key !classes;
              incr classes;
              !classes - 1
        in
        Bytes.set class_ c (Char.chr cl)
      done;
      let shift = ref 0 in
      while 1 lsl !shift < !classes do
        incr shift
      done;
      let shift = !shift in
      let stride = 1 lsl shift in
      (* One unit of each class, to move a set by. *)
      let sample = Array.make 129 0 in
      for c = 127 downto 0 do
        sample.(Char.code (Bytes.get class_ c)) <- c
      done;
      (* The search is unanchored: a match may start at any position, so after every unit the states the pattern starts
         in (not at the start of the text) join the set. *)
      let restart = closure [ start ] false false None in
      let ids = Hashtbl.create 64 in
      let sets = ref (Array.make 16 ([], false, false)) and nsets = ref first_state in
      let trans = Buffer.create (first_state * stride * 4) in
      for st = 0 to first_state - 1 do
        for _ = 0 to stride - 1 do
          Buffer.add_char trans (Char.chr (if st = accept then accept else if st = bail then bail else dead))
        done
      done;
      let at_end = ref [ false; false; false ] in
      (* Gives a set its state. *)
      let intern set word at_start =
        if has_match set then accept
        else if set = [] then dead
        else begin
          let key = (set, word, at_start) in
          match Hashtbl.find_opt ids key with
          | Some id -> id
          | None ->
              if !nsets >= max_dfa_states then raise Too_large;
              let id = !nsets in
              Hashtbl.replace ids key id;
              if id >= Array.length !sets then begin
                let bigger = Array.make (2 * id) ([], false, false) in
                Array.blit !sets 0 bigger 0 id;
                sets := bigger
              end;
              !sets.(id) <- key;
              incr nsets;
              for _ = 0 to stride - 1 do
                Buffer.add_char trans '\000'
              done;
              (* At the end of the text the end anchors are passed, and a boundary is between a unit and none. *)
              at_end := has_match (closure set at_start true (Some (word, false))) :: !at_end;
              id
        end
      in
      try
        let first = closure [ start ] true false None in
        let s0 = intern first false true in
        let tbl = ref Bytes.empty in
        let id = ref first_state in
        let rows = Buffer_int.create 256 in
        while !id < !nsets do
          let set, word, at_start = !sets.(!id) in
          for cl = 0 to stride - 1 do
            let to_ =
              if cl = 0 || cl >= !classes then bail (* Not ASCII (or no such class). *)
              else begin
                let c = sample.(cl) in
                (* The boundaries at this position are passed now that the unit after it is known. *)
                let resolved = closure set at_start false (Some (word, is_word c)) in
                if has_match resolved then accept
                else begin
                  let moved =
                    List.filter_map (fun i -> let s = nfa.(i) in if s.nk = NSet && s.nset.[c] <> '\000' then Some s.out else None) resolved
                  in
                  let next = closure (moved @ restart) false false None in
                  let to_ = intern next (is_word c) false in
                  if !steps < 0 then raise Too_large;
                  to_
                end
              end
            in
            Buffer_int.add rows ((!id * stride) + cl);
            Buffer_int.add rows to_
          done;
          incr id
        done;
        tbl := Buffer.to_bytes trans;
        let r = Buffer_int.contents rows in
        let k = ref 0 in
        while !k < Array.length r do
          Bytes.set !tbl r.(!k) (Char.chr r.(!k + 1));
          k := !k + 2
        done;
        let at_end = Array.of_list (List.rev !at_end) in
        Some
          {
            class_ = Bytes.sub_string class_ 0 128;
            trans = Bytes.to_string !tbl;
            shift;
            start = s0;
            at_end;
            empty = s0 = accept || at_end.(s0);
          }
      with Too_large -> None)

(* The largest program a linear matcher is asked to compile, in the instructions estimated for it before it is written
   (see re2_cost). A counted repetition is written out, so a{1000} is a thousand instructions. *)
let max_re2_cost = 10_000

(* Estimates the number of instructions of the program a linear matcher compiles for a tree (re2Cost of the Go engine,
   re2.rs of the Rust crate). A counted repetition is written out: x{n,m} is m copies of x, and x{n,} is n copies and a
   loop. The estimate stops counting above max_re2_cost. *)
let rec re2_cost n =
  match n.kind with
  | Empty -> 0
  | Cat | Alt | Group ->
      let cost = ref (Array.length n.subs - 1) and k = ref 0 in
      while !k < Array.length n.subs && !cost <= max_re2_cost do
        cost := !cost + re2_cost n.subs.(!k);
        incr k
      done;
      if !cost > max_re2_cost then max_re2_cost + 1 else !cost
  | Repeat ->
      let copies = max 1 (if n.max = unbounded then n.min else n.max) in
      (* Each copy has the body and one instruction for the choice to enter it. *)
      let body = re2_cost n.subs.(0) + 1 in
      if copies > max_re2_cost || body * copies > max_re2_cost then max_re2_cost + 1 else body * copies
  | _ -> 1
