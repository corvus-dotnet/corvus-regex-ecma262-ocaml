(* An anchored, leftmost-first automaton over ASCII text, which finds where the match that starts at a position ends for
   a pattern that needs no backtracking (adfa.rs of the Rust crate): what the regex crate's DFAs do for the match, taken
   into this engine where ECMA-262's semantics allow it.

   A state is an ordered list of the pattern's states (its threads, the most preferred first, as the backtracking
   matcher would try them), with whether the unit before it was a word character and whether it is at the start of the
   text. When the match state is among the threads, a match ends here, and the threads after it, which the backtracking
   matcher would only try had this way failed, are dropped; the threads before it may still find a longer match, which
   it would prefer. So the last match found before the threads run out is the backtracking matcher's match, for a
   pattern with no lookaround, no backreference and no repetition whose body can match the empty string. A unit beyond
   ASCII hands the start back to the backtracking matcher.

   Every state is worked out when the pattern is compiled, within bounds of states and steps, so that a pattern too
   large for an automaton costs little before that is known. This module builds the automata; the loops that read them
   over a text are in the matcher (vm.ml.in), written once for each encoding. *)

open Parse

let max_states = 2048
let max_nfa = 1024
let max_steps = 2_000_000

(* No thread is left: no match can end further on. *)
let dead = 0

(* The flag of a transition across which a match ends before the unit, in the table being built and in the
   premultiplied one. *)
let matched = 0x8000

let fast_matched = 0x80000000

type skind = KSet | KSplit | KBol | KEol | KWord | KNot_word | KMatch

(* A state of the pattern: its set as 128 booleans (for KSet), whether the set holds a character beyond ASCII, which the
   automaton gives up at, and where it goes. *)
type state = { sk : skind; set : string; wide : bool; mutable out : int; mutable alt : int }

let no_set = String.make 128 '\000'
let state sk out = { sk; set = no_set; wide = false; out; alt = 0 }

type builder = { mutable states : state array; mutable count : int; mutable failed : bool }

let builder () = { states = Array.make 64 (state KMatch 0); count = 0; failed = false }

let add b s =
  if b.count >= max_nfa then begin
    b.failed <- true;
    0
  end
  else begin
    if b.count = Array.length b.states then begin
      let bigger = Array.make (2 * b.count) (state KMatch 0) in
      Array.blit b.states 0 bigger 0 b.count;
      b.states <- bigger
    end;
    b.states.(b.count) <- s;
    b.count <- b.count + 1;
    b.count - 1
  end

(* A split whose preferred way is first. *)
let split b first second = add b { (state KSplit first) with alt = second }

let set_of_char r = if r >= 0 && r < 128 then String.init 128 (fun c -> if c = r then '\001' else '\000') else no_set

(* The states of a node that go on to next; the state they start at. *)
let rec compile b n next =
  if b.failed then 0
  else
    match n.kind with
    | Empty -> next
    | Char -> add b { (state KSet next) with set = set_of_char n.r; wide = n.r >= 128 }
    | Set ->
        let cs = class_of n in
        let r = cs.ranges in
        let wide = Array.length r > 0 && r.(Array.length r - 1) >= 128 in
        add b { (state KSet next) with set = cs.ascii; wide }
    | Cat ->
        let next = ref next in
        for k = Array.length n.subs - 1 downto 0 do
          next := compile b n.subs.(k) !next
        done;
        !next
    | Alt ->
        let last = Array.length n.subs - 1 in
        let start = ref (compile b n.subs.(last) next) in
        for k = last - 1 downto 0 do
          let out = compile b n.subs.(k) next in
          start := split b out !start
        done;
        !start
    | Group -> compile b n.subs.(0) next
    | Repeat ->
        let sub = n.subs.(0) in
        if n.max <> 0 && n.max <> 1 && Compile.nullable sub then begin
          (* ECMA-262 refuses an empty iteration after the least count, which a thread does not know. *)
          b.failed <- true;
          0
        end
        else begin
          let next = ref next in
          if n.max = Parse.unbounded then begin
            let loop_ = split b 0 0 in
            if not b.failed then begin
              let body = compile b sub loop_ in
              let s = b.states.(loop_) in
              if n.lazy_ then begin
                s.out <- !next;
                s.alt <- body
              end
              else begin
                s.out <- body;
                s.alt <- !next
              end;
              next := loop_
            end
          end
          else begin
            let i = ref n.min in
            while !i < n.max && not b.failed do
              let body = compile b sub !next in
              next := if n.lazy_ then split b !next body else split b body !next;
              incr i
            done
          end;
          let i = ref 0 in
          while !i < n.min && not b.failed do
            next := compile b sub !next;
            incr i
          done;
          !next
        end
    | Bol -> add b (state KBol next)
    | Eol -> add b (state KEol next)
    | Word_boundary when not n.fold -> add b (state KWord next)
    | Not_word_boundary when not n.fold -> add b (state KNot_word next)
    | _ ->
        b.failed <- true;
        0

(* The states of a node read backwards that go on to next. *)
let rec compile_back b n next =
  if b.failed then 0
  else
    match n.kind with
    | Cat ->
        let next = ref next in
        Array.iter (fun sub -> next := compile_back b sub !next) n.subs;
        !next
    | Alt ->
        let last = Array.length n.subs - 1 in
        let start = ref (compile_back b n.subs.(last) next) in
        for k = last - 1 downto 0 do
          let out = compile_back b n.subs.(k) next in
          start := split b out !start
        done;
        !start
    | Group -> compile_back b n.subs.(0) next
    | Repeat ->
        let sub = n.subs.(0) in
        let next = ref next in
        if n.max = Parse.unbounded then begin
          let loop_ = split b 0 0 in
          if not b.failed then begin
            let body = compile_back b sub loop_ in
            let s = b.states.(loop_) in
            s.out <- body;
            s.alt <- !next;
            next := loop_
          end
        end
        else begin
          let i = ref n.min in
          while !i < n.max && not b.failed do
            let body = compile_back b sub !next in
            next := split b body !next;
            incr i
          done
        end;
        let i = ref 0 in
        while !i < n.min && not b.failed do
          next := compile_back b sub !next;
          incr i
        done;
        !next
    | Empty | Char | Set -> compile b n next
    | _ ->
        b.failed <- true;
        0

(* The anchored automaton of a pattern. *)
type anchored = {
  (* The class of each unit below 128. *)
  class_ : string;
  shift : int;
  (* fast.(state * stride + class): the next state, premultiplied by the stride, with fast_matched when a match ends
     before the unit. *)
  fast : int array;
  (* For each state: a match ends at the end of the text. *)
  at_end : bool array;
  (* The start state for each of: the unit before is a word character (bit 1), the start of the text (bit 0). *)
  starts : int array;
  (* Whether the start states differ (the pattern looks back at the unit before or at the start of the text). *)
  context : bool;
  (* For the forward automaton, whether each state is a start state: no thread of an earlier start is alive in it. *)
  start_like : bool array;
}

type det = { nfa : state array; mark : int array; mutable generation : int; mutable steps : int }

(* Appends to out, in order of preference, the threads that reading nothing reaches from from, keeping the states that
   read a unit, the anchors and boundaries still to pass, and the match, after which nothing more is kept. Boundaries
   are passed or dropped with words (before, after), and kept to pass later without it. *)
let closure_on d (out : Buffer_int.t) (from : int list) at_start at_end (words : (bool * bool) option) =
  let stack = ref from and fin = ref false in
  while (not !fin) && !stack <> [] do
    let i = List.hd !stack in
    stack := List.tl !stack;
    d.steps <- d.steps - 1;
    if d.mark.(i) <> d.generation then begin
      d.mark.(i) <- d.generation;
      let s = d.nfa.(i) in
      match s.sk with
      | KSplit -> stack := s.out :: s.alt :: !stack
      | KBol -> if at_start then stack := s.out :: !stack
      | KEol -> if at_end then stack := s.out :: !stack else Buffer_int.add out i
      | KWord | KNot_word -> (
          match words with
          | None -> Buffer_int.add out i
          | Some (before, after) -> if before <> after = (s.sk = KWord) then stack := s.out :: !stack)
      | KMatch ->
          Buffer_int.add out i;
          fin := true
      | KSet -> Buffer_int.add out i
    end
  done

let closure d out from at_start at_end words =
  d.generation <- d.generation + 1;
  closure_on d out from at_start at_end words

let is_word c = Charset.is_word_byte c
let in_set (s : state) c = String.unsafe_get s.set c <> '\000'

(* The classes of units: those every set has both or neither of, and (with words) that are both word characters or
   neither. It returns the class of each unit, the number of classes, and a unit of each class. *)
let classes (nfa : state array) words =
  let class_ = Bytes.make 128 '\000' and signatures = Hashtbl.create 64 in
  for c = 0 to 127 do
    let sig_ = Buffer.create 64 in
    if words then Buffer.add_char sig_ (if is_word c then '1' else '0');
    Array.iter (fun s -> if s.sk = KSet then Buffer.add_char sig_ (if in_set s c then '1' else '0')) nfa;
    let key = Buffer.contents sig_ in
    let id =
      match Hashtbl.find_opt signatures key with
      | Some id -> id
      | None ->
          let id = Hashtbl.length signatures in
          Hashtbl.replace signatures key id;
          id
    in
    Bytes.set class_ c (Char.chr id)
  done;
  let n = Hashtbl.length signatures in
  let sample = Array.make n 0 in
  for c = 127 downto 0 do
    sample.(Char.code (Bytes.get class_ c)) <- c
  done;
  let shift = ref 0 in
  while 1 lsl !shift < n do
    incr shift
  done;
  (Bytes.to_string class_, n, sample, !shift)

exception Too_large

let build_with root forward =
  let b = builder () in
  let match_ = add b (state KMatch 0) in
  let start = compile b root match_ in
  if b.failed then None
  else begin
    let nfa = Array.sub b.states 0 b.count in
    let n = Array.length nfa in
    let d = { nfa; mark = Array.make n 0; generation = 0; steps = max_steps } in
    (* In the forward automaton a thread of the start joined at the current unit is kept apart from the threads of
       earlier starts, as its state's number plus the number of states, so that a state whose threads are all of the
       current start (a start state) is known: no earlier start can still match. *)
    let decode set = List.map (fun i -> if i >= n then i - n else i) set in
    let class_, nclasses, sample, shift = classes nfa true in
    let stride = 1 lsl shift in
    let has (set : int list) k = List.exists (fun i -> nfa.(i).sk = k) set in
    (* The word bit matters only to a pattern with a boundary and the start bit only to one with a start anchor. *)
    let boundaries = Array.exists (fun s -> s.sk = KWord || s.sk = KNot_word) nfa in
    let anchors = Array.exists (fun s -> s.sk = KBol) nfa in
    let ids = Hashtbl.create 64 in
    let sets = ref [| ([], false, false) |] and nsets = ref 1 in
    let trans = Buffer_int.create (16 * stride) in
    for _ = 1 to stride do
      Buffer_int.add trans dead
    done;
    let at_end = ref [ false ] in
    let push_set key =
      if !nsets = Array.length !sets then begin
        let bigger = Array.make (2 * !nsets) ([], false, false) in
        Array.blit !sets 0 bigger 0 !nsets;
        sets := bigger
      end;
      !sets.(!nsets) <- key;
      incr nsets
    in
    let intern set word at_start =
      if set = [] then dead
      else begin
        let word = word && boundaries and at_start = at_start && anchors in
        let key = (set, word, at_start) in
        match Hashtbl.find_opt ids key with
        | Some id -> id
        | None ->
            if !nsets >= max_states then raise Too_large;
            let id = !nsets in
            let closed = Buffer_int.create 16 in
            closure d closed (decode set) at_start true (Some (word, false));
            at_end := has (Array.to_list (Buffer_int.contents closed)) KMatch :: !at_end;
            for _ = 1 to stride do
              Buffer_int.add trans dead
            done;
            Hashtbl.replace ids key id;
            push_set key;
            id
      end
    in
    match
      let starts =
        Array.init 4 (fun k ->
            let word = k land 2 <> 0 and at_start = k land 1 <> 0 in
            let first = Buffer_int.create 16 in
            closure d first [ start ] at_start false None;
            let first = Array.to_list (Buffer_int.contents first) in
            let first = if forward then List.map (fun i -> i + n) first else first in
            intern first word at_start)
      in
      let id = ref 1 in
      let resolved = Buffer_int.create 16 in
      while !id < !nsets do
        let set, word, at_start = !sets.(!id) in
        let set = decode set in
        for cl = 0 to nclasses - 1 do
          let c = sample.(cl) in
          Buffer_int.clear resolved;
          closure d resolved set at_start false (Some (word, is_word c));
          let resolved = Array.to_list (Buffer_int.contents resolved) in
          let flag = ref 0 in
          let resolved =
            (* A match ends before this unit; only the threads it would have been tried after go on. *)
            let rec take = function
              | [] -> []
              | i :: rest ->
                  if nfa.(i).sk = KMatch then begin
                    flag := matched;
                    []
                  end
                  else i :: take rest
            in
            take resolved
          in
          let moved = List.filter_map (fun i -> let s = nfa.(i) in if s.sk = KSet && in_set s c then Some s.out else None) resolved in
          let next = Buffer_int.create 16 in
          closure d next moved false false None;
          let next = Array.to_list (Buffer_int.contents next) in
          let next =
            if forward && !flag = 0 then begin
              (* The fresh start's threads, behind the moved ones and told apart from them. *)
              let fresh = Buffer_int.create 16 in
              closure_on d fresh [ start ] false false None;
              next @ List.map (fun i -> i + n) (Array.to_list (Buffer_int.contents fresh))
            end
            else next
          in
          let to_ = intern next (is_word c) false in
          if d.steps < 0 then raise Too_large;
          Buffer_int.set trans ((!id * stride) + cl) (to_ lor !flag)
        done;
        incr id
      done;
      starts
    with
    | exception Too_large -> None
    | starts ->
        let fast =
          Array.map
            (fun t -> ((t land lnot matched) lsl shift) lor if t land matched <> 0 then fast_matched else 0)
            (Buffer_int.contents trans)
        in
        let context = Array.exists (fun s -> s <> starts.(0)) starts in
        let start_like =
          if forward then
            Array.init !nsets (fun k ->
                let set, _, _ = !sets.(k) in
                set <> [] && List.for_all (fun i -> i >= n) set)
          else [||]
        in
        Some { class_; shift; fast; at_end = Array.of_list (List.rev !at_end); starts; context; start_like }
  end

(* The anchored automaton of a tree (after simplify), or None when the pattern has none. *)
let build root = build_with root false

(* The forward automaton of a tree: the anchored automaton with a new start joined at every unit, behind every thread
   already alive (so last in preference, as the search tries later starts after earlier ones). Read over the text from a
   start, it says where the first match ends, and its start states say where no thread of an earlier start is alive:
   the leftmost match starts at or after the last of them, the forward search of the regex crate. *)
let build_forward root = build_with root true

(* The state after the prefix from each start state, premultiplied by the stride (0 where no match can start with it),
   or None when a match can end inside it or it holds a byte beyond ASCII: what a search goes on from where a match is
   known to start with the prefix. *)
let after a (prefix : string) =
  try
    Some
      (Array.map
         (fun s ->
           let at = ref (s lsl a.shift) and k = ref 0 in
           while !at <> 0 && !k < String.length prefix do
             let c = Char.code prefix.[!k] in
             if c >= 0x80 then raise Exit;
             let t = a.fast.(!at + Char.code a.class_.[c]) in
             if t land fast_matched <> 0 then raise Exit;
             at := t;
             incr k
           done;
           !at)
         a.starts)
  with Exit -> None

(* The positions of a pattern read before a match can end: V8's Boyer-Moore lookahead. For a window of positions lo..hi
   of every match, the units each can be (table for ASCII, wide for any other) are joined; a unit at hi from a place
   that is none of them rules out the starts from that place to hi - lo units on. Over UTF-8, where the window's bytes
   are up to four ranges (ranges, a least byte and a width each, nranges of them; the runs of the table, any byte
   beyond ASCII where wide, the nearest runs joined until four are left), the search looks for a run of them as long as
   the window (Kernels.index_run): a start has one of the window's bytes at each of its places. *)
type skip = { hi : int; len : int; table : string; swide : bool; ranges : string; nranges : int }

let[@inline] skip_hit s c = if c < 128 then String.unsafe_get s.table c <> '\000' else s.swide

(* The most positions looked at for a window. *)
let skip_positions = 16

(* How often a unit is found in text, roughly: what a window's worth is reckoned by. *)
let frequency c =
  let ch = Char.chr c in
  if ch = ' ' then 0.15
  else if ch >= 'a' && ch <= 'z' then 0.06
  else if ch = '\n' || ch = '\r' || ch = '.' || ch = ',' then 0.015
  else if (ch >= 'A' && ch <= 'Z') || (ch >= '0' && ch <= '9') then 0.004
  else 0.003

let window_ranges table wide =
  let runs = ref [] and c = ref 0 in
  while !c < 128 do
    if table.(!c) then begin
      let x = !c in
      while !c + 1 < 128 && table.(!c + 1) do
        incr c
      done;
      runs := (x, !c) :: !runs
    end;
    incr c
  done;
  let runs = List.rev !runs @ if wide then [ (0x80, 0xFF) ] else [] in
  let runs = ref (Array.of_list runs) in
  while Array.length !runs > 4 do
    (* The two runs with the least between them become one. *)
    let r = !runs in
    let best = ref 0 in
    for k = 1 to Array.length r - 2 do
      if fst r.(k + 1) - snd r.(k) < fst r.(!best + 1) - snd r.(!best) then best := k
    done;
    let merged = Array.make (Array.length r - 1) (0, 0) in
    for k = 0 to Array.length merged - 1 do
      merged.(k) <- (if k < !best then r.(k) else if k = !best then (fst r.(k), snd r.(k + 1)) else r.(k + 1))
    done;
    runs := merged
  done;
  let r = !runs in
  (String.init (2 * Array.length r) (fun k -> let lo, hi = r.(k / 2) in Char.chr (if k mod 2 = 0 then lo else hi - lo)), Array.length r)

(* The window of a tree (after simplify) worth reading one unit of instead of every unit, or None: the units each
   position before a match can end can be, from the pattern's automaton without its determinisation, and the window
   whose units are rare enough that one unit read passes over three starts or more, reckoned by frequency. *)
let skip_window root =
  let b = builder () in
  let match_ = add b (state KMatch 0) in
  let start = compile b root match_ in
  if b.failed then None
  else begin
    let nfa = Array.sub b.states 0 b.count in
    (* The states each depth's threads are at, assertions taken as passed (they only rule threads out). *)
    let closure from =
      let seen = Array.make (Array.length nfa) false and stack = ref from and sets = ref [] and ends = ref false in
      while (not !ends) && !stack <> [] do
        let i = List.hd !stack in
        stack := List.tl !stack;
        if not seen.(i) then begin
          seen.(i) <- true;
          match nfa.(i).sk with
          (* A match can end here: no later position is known. *)
          | KMatch -> ends := true
          | KSet -> sets := i :: !sets
          | KSplit -> stack := nfa.(i).out :: nfa.(i).alt :: !stack
          | KBol | KEol | KWord | KNot_word -> stack := nfa.(i).out :: !stack
        end
      done;
      if !ends then None else Some (List.rev !sets)
    in
    let positions = ref [] and threads = ref [ start ] and fin = ref false in
    while (not !fin) && List.length !positions < skip_positions do
      match closure !threads with
      | None -> fin := true
      | Some sets ->
          let table = Array.make 128 false and wide = ref false in
          List.iter
            (fun i ->
              for c = 0 to 127 do
                if in_set nfa.(i) c then table.(c) <- true
              done;
              if nfa.(i).wide then wide := true)
            sets;
          positions := !positions @ [ (table, !wide) ];
          (* A character beyond ASCII takes more than one unit of UTF-8: the positions after it are not known. *)
          if !wide then fin := true else threads := List.map (fun i -> nfa.(i).out) sets
    done;
    let positions = Array.of_list !positions in
    let best = ref None in
    for lo = 0 to Array.length positions - 1 do
      let table = Array.make 128 false and wide = ref false in
      for hi = lo to Array.length positions - 1 do
        let t, w = positions.(hi) in
        for c = 0 to 127 do
          if t.(c) then table.(c) <- true
        done;
        if w then wide := true;
        let p = ref (if !wide then 0.1 else 0.0) in
        for c = 0 to 127 do
          if table.(c) then p := !p +. frequency c
        done;
        let worth = float_of_int (hi - lo + 1) *. (1.0 -. Float.min !p 1.0) in
        if worth >= 3.0 && match !best with None -> true | Some (w, _, _) -> worth > w then best := Some (worth, lo, hi)
      done
    done;
    match !best with
    | None -> None
    | Some (_, lo, hi) ->
        let table = Array.make 128 false and wide = ref false in
        for k = lo to hi do
          let t, w = positions.(k) in
          for c = 0 to 127 do
            if t.(c) then table.(c) <- true
          done;
          if w then wide := true
        done;
        let ranges, nranges = window_ranges table !wide in
        Some
          {
            hi;
            len = hi - lo + 1;
            table = String.init 128 (fun c -> if table.(c) then '\001' else '\000');
            swide = !wide;
            ranges;
            nranges;
          }
  end

(* An automaton of the terms before the anchors of a search, read backwards (accepting where any of them can start):
   from an occurrence of an anchor it finds, over ASCII text, every start from which some alternative's terms reach it.
   Only terms with no assertion are taken. *)
type reverse = {
  rclass : string;
  rshift : int;
  (* The transitions, each the next state's row (premultiplied by the stride, so 0 is the dead state) with fast_matched
     where that state accepts. *)
  rfast : int array;
  (* Whether the first state accepts (an empty run of terms). *)
  accept_first : bool;
}

(* The reverse automaton of the prefixes of a search's anchors, or None when one holds an assertion or the automaton is
   too large. *)
let reverse (prefixes : node array list) =
  let b = builder () in
  let match_ = add b (state KMatch 0) in
  let starts =
    List.map
      (fun prefix ->
        let next = ref match_ in
        Array.iter (fun t -> next := compile_back b t !next) prefix;
        !next)
      prefixes
  in
  if b.failed || starts = [] then None
  else begin
    let starts = Array.of_list starts in
    let start = ref starts.(Array.length starts - 1) in
    for k = Array.length starts - 2 downto 0 do
      start := split b starts.(k) !start
    done;
    if b.failed then None
    else begin
      let nfa = Array.sub b.states 0 b.count in
      let d = { nfa; mark = Array.make (Array.length nfa) 0; generation = 0; steps = max_steps } in
      let class_, nclasses, sample, shift = classes nfa false in
      let stride = 1 lsl shift in
      (* A set of states, sorted, is its key; every thread is kept (any of them reaching the match is a start). *)
      let closure from =
        d.generation <- d.generation + 1;
        let out = ref [] and stack = ref from in
        while !stack <> [] do
          let i = List.hd !stack in
          stack := List.tl !stack;
          d.steps <- d.steps - 1;
          if d.mark.(i) <> d.generation then begin
            d.mark.(i) <- d.generation;
            let s = nfa.(i) in
            match s.sk with KSplit -> stack := s.out :: s.alt :: !stack | _ -> out := i :: !out
          end
        done;
        List.sort_uniq compare !out
      in
      let ids = Hashtbl.create 64 and sets = ref [| [] |] and nsets = ref 1 in
      let trans = Buffer_int.create (16 * stride) in
      for _ = 1 to stride do
        Buffer_int.add trans dead
      done;
      let accept = ref [ false ] in
      let push key =
        if !nsets = Array.length !sets then begin
          let bigger = Array.make (2 * !nsets) [] in
          Array.blit !sets 0 bigger 0 !nsets;
          sets := bigger
        end;
        !sets.(!nsets) <- key;
        incr nsets;
        accept := List.exists (fun i -> nfa.(i).sk = KMatch) key :: !accept;
        for _ = 1 to stride do
          Buffer_int.add trans dead
        done
      in
      let first = closure [ !start ] in
      Hashtbl.replace ids first 1;
      push first;
      match
        let id = ref 1 in
        while !id < !nsets do
          for cl = 0 to nclasses - 1 do
            let c = sample.(cl) in
            let moved = List.filter_map (fun i -> let s = nfa.(i) in if s.sk = KSet && in_set s c then Some s.out else None) !sets.(!id) in
            let next = closure moved in
            let to_ =
              if next = [] then dead
              else
                match Hashtbl.find_opt ids next with
                | Some t -> t
                | None ->
                    if !nsets >= max_states then raise Too_large;
                    let t = !nsets in
                    Hashtbl.replace ids next t;
                    push next;
                    t
            in
            if d.steps < 0 then raise Too_large;
            Buffer_int.set trans ((!id * stride) + cl) to_
          done;
          incr id
        done
      with
      | exception Too_large -> None
      | () ->
          let accept = Array.of_list (List.rev !accept) in
          let rfast =
            Array.map
              (fun t -> (t lsl shift) lor if t <> dead && accept.(t) then fast_matched else 0)
              (Buffer_int.contents trans)
          in
          Some { rclass = class_; rshift = shift; rfast; accept_first = accept.(1) }
    end
  end
