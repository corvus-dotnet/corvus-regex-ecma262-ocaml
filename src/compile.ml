(* The compiler: a syntax tree to a program of the backtracking matcher, with what the matcher needs to give up early
   (compile.rs of the Rust crate). *)

open Parse

type op =
  (* Char and Set consume one code point going forward. The Back forms consume the one before the position, which is
     how the body of a lookbehind runs. *)
  | Char_
  | Char_back
  | Set_
  | Set_back
  (* Consumes a run of literal units going forward. *)
  | Lit
  | Fail
  | Bol_
  | Eol_
  | Word_boundary_
  | Not_word_boundary_
  (* Line_start and Line_end are ^ and $ with the m modifier. The Fold forms of the word boundaries and of the
     backreferences belong to a case-insensitive group. *)
  | Line_start_
  | Line_end_
  | Word_boundary_fold
  | Not_word_boundary_fold
  | Jmp
  (* Continues at x and leaves y as the alternative. *)
  | Split
  (* Save_tmp notes where a group was entered. Capture records the group once it has closed. *)
  | Save_tmp
  | Capture
  (* Resets the groups a to b, as ECMA-262 does at the start of each iteration of a quantified body. *)
  | Clear
  | Backref_
  | Backref_back
  | Backref_fold
  | Backref_fold_back
  (* Backref_units and Backref_units_back compare a group a code unit at a time, for a pattern matched by code unit. *)
  | Backref_units
  | Backref_units_back
  (* Look opens a lookaround. Look_ok ends the body of a positive one and Look_fail the body of a negative one. *)
  | Look_
  | Look_ok
  | Look_fail
  (* Rep and Rep_lazy repeat one character or class without entering the general loop. *)
  | Rep
  | Rep_lazy
  (* Loop_init, Loop and Loop_iter run a counted or possibly empty quantified body. *)
  | Loop_init
  | Loop
  | Loop_iter
  | Match

(* One instruction of the backtracking matcher. *)
type inst = {
  op : op;
  (* Marks a lazy Loop, a backward Capture, a negative Look, and a Backref_fold that folds case as the u flag grammar
     does. *)
  mutable flag : bool;
  mutable r : int;
  mutable set : Charset.t option;
  (* The literal run of a Lit, as UTF-8 bytes and as UTF-16LE bytes. *)
  mutable lit : string;
  mutable lit16 : string;
  (* For a Rep, the ASCII bytes (up to three) what follows it can start with, when those are all it can start with:
     its give-back looks for them (see set_back). *)
  mutable back : string;
  (* x and y are jump targets. *)
  mutable x : int;
  mutable y : int;
  (* a and b are a group, a loop or a range of groups, depending on the instruction. For a backreference b is the major
     number of the version of Unicode whose case folding it matches with. *)
  mutable a : int;
  mutable b : int;
  (* min and max bound a repetition. A max of -1 is no bound. *)
  mutable min : int;
  mutable max : int;
  (* The least number of bytes of text that every way from this instruction to a match consumes going forward (see
     set_needs). It is set for a Loop, and counts none of the iterations the loop has still to make. need16 is the same
     in UTF-16 code units. *)
  mutable need : int;
  mutable need16 : int;
  (* The least number of bytes one iteration of the body of a Loop consumes going forward, and iter16 the least number
     of UTF-16 code units. *)
  mutable iter : int;
  mutable iter16 : int;
  (* The number of the instruction's memo point, counted from 1, or 0 when it has none. It is set for a Split, a Loop,
     a Rep and a Rep_lazy. *)
  mutable m : int;
}

let inst op =
  {
    op;
    flag = false;
    r = 0;
    set = None;
    lit = "";
    lit16 = "";
    back = "";
    x = 0;
    y = 0;
    a = 0;
    b = 0;
    min = 0;
    max = 0;
    need = 0;
    need16 = 0;
    iter = 0;
    iter16 = 0;
    m = 0;
  }

(* The number of loops whose state a memo point can tell apart. *)
let max_memo_loops = 2

(* A place where the matcher makes a choice and can remember that the choice has failed: a split, a loop that may be
   entered or left, and what follows a single character loop that gives characters back or takes more. Whether a way
   from such a place leads to a match depends on the place and the position alone when nothing else the matcher keeps
   can change what follows. So a pattern with a backreference has no memo points, and neither has a place inside a loop
   that counts its iterations up to a bound. Inside a loop with no upper bound and a least count of zero or one, one
   thing more decides what follows: whether the loop would refuse the iteration as empty if it ended here. That is one
   bit for each such loop the place is in (loops, innermost last), and a place in more than max_memo_loops of them has
   no memo point. *)
type memo_point = { loops : int array; mins : int array }

(* One counted loop that the instruction being written is inside of: its number, its least count, and whether its state
   is one bit to a memo point (no upper bound, and a least count of zero or one). *)
type loop_context = { loop_ : int; lmin : int; simple : bool }

(* What can come next on a way from an instruction to the end of its match, which is the end of the pattern or of the
   lookaround the instruction is in. The matcher looks at it before it takes a way: when the next unit of the text is
   none that the way can start with, and the way cannot end without one, the way fails, so it is not taken, and where it
   was one of two no choice is kept for it. V8 makes the same check ("quick check") at its choices.

   The units below 128 a way can consume next are four words of 32 bits (q0 for 0 to 31, and so on). other says it can
   consume a unit that is not ASCII; mid that a way reaches the end of its match consuming nothing more while text is
   left, and end_ that one does at the end of the text. *)
type quick = { q0 : int; q1 : int; q2 : int; q3 : int; other : bool; mid : bool; end_ : bool }

let no_quick = { q0 = 0; q1 = 0; q2 = 0; q3 = 0; other = false; mid = false; end_ = false }
let all_quick = { q0 = 0xFFFFFFFF; q1 = 0xFFFFFFFF; q2 = 0xFFFFFFFF; q3 = 0xFFFFFFFF; other = true; mid = true; end_ = true }

(* Whether the quick holds the ASCII unit c. *)
let[@inline] quick_has q c =
  let w = match c lsr 5 with 0 -> q.q0 | 1 -> q.q1 | 2 -> q.q2 | _ -> q.q3 in
  (w lsr (c land 31)) land 1 <> 0

let quick_add_rune q r =
  if r >= 0 && r < 0x80 then begin
    let bit = 1 lsl (r land 31) in
    match r lsr 5 with
    | 0 -> { q with q0 = q.q0 lor bit }
    | 1 -> { q with q1 = q.q1 lor bit }
    | 2 -> { q with q2 = q.q2 lor bit }
    | _ -> { q with q3 = q.q3 lor bit }
  end
  else { q with other = true }

let quick_add_set q (s : Charset.t) =
  let q = ref q in
  for c = 0 to 127 do
    if Charset.has_ascii s c then q := quick_add_rune !q c
  done;
  let n = Array.length s.ranges in
  if n > 0 && s.ranges.(n - 1) >= 0x80 then { !q with other = true } else !q

let quick_union a b =
  {
    q0 = a.q0 lor b.q0;
    q1 = a.q1 lor b.q1;
    q2 = a.q2 lor b.q2;
    q3 = a.q3 lor b.q3;
    other = a.other || b.other;
    mid = a.mid || b.mid;
    end_ = a.end_ || b.end_;
  }

(* A pattern compiled for the backtracking matcher. *)
type program = {
  insts : inst array;
  (* The number of groups the program records (every capturing group, or those some backreference names), and the
     number of counted loops. *)
  groups : int;
  loops : int;
  (* The pattern was read without the u flag, so the text is matched by UTF-16 code unit (see Units). *)
  units : bool;
  (* Every match starts at the start of the text. *)
  anchored : bool;
  (* The least number of bytes a match consumes (see set_needs), and of UTF-16 code units. *)
  need : int;
  need16 : int;
  (* The memo points of the program. *)
  points : memo_point array;
  (* For each instruction, which units can come next on a way from it to the end of its match (see set_quick). *)
  quick : quick array;
  (* Every code point a match can start with. None when a match can be empty or when the set is not known. *)
  first : Charset.t option;
}

type compiler = {
  mutable cinsts : inst array;
  mutable count : int;
  (* Maps a capturing group's number to its slot, for the groups the program records. The others are not recorded at
     all. *)
  tracked : (int, int) Hashtbl.t;
  mutable cloops : int;
  (* The pattern was read with the u flag grammar, which decides how a backreference folds case. *)
  cunicode : bool;
  (* memo says the pattern has no backreference, so its choices can have memo points. open_ lists the counted loops
     around the instruction being written, outermost first, inside the innermost lookaround: what a lookaround's body
     does depends on nothing outside it. cpoints collects the memo points. *)
  mutable memo : bool;
  mutable open_ : loop_context list;
  mutable cpoints : memo_point list;
  mutable npoints : int;
}

(* Whether a code point can be matched by comparing its UTF-8 bytes. A surrogate has no encoding, and U+FFFD is also
   what a malformed byte of the text decodes to. *)
let literal_rune r = r >= 0 && r <= 0x10FFFF && not (r >= 0xD800 && r <= 0xDFFF) && r <> Units.rune_error

(* Whether n can match without consuming anything. It errs towards true. *)
let rec nullable n =
  match n.kind with
  | Char | Set -> false
  | Cat -> Array.for_all nullable n.subs
  | Alt -> Array.exists nullable n.subs
  | Group -> nullable n.subs.(0)
  | Repeat -> n.min = 0 || nullable n.subs.(0)
  | _ -> true

(* Whether a node holds a capturing group, a backreference or a lookaround. *)
let rec has_capture n =
  (n.kind = Group && n.group > 0) || n.kind = Backref || n.kind = Look || Array.exists has_capture n.subs

(* The number of nodes of a tree. *)
let rec size n = Array.fold_left (fun acc s -> acc + size s) 1 n.subs

(* Whether n can match one character or more. It errs towards true. *)
let rec consumes n =
  match n.kind with
  | Char | Set | Backref -> true
  | Cat | Alt | Group -> Array.exists consumes n.subs
  | Repeat -> n.max <> 0 && consumes n.subs.(0)
  | _ -> false

let rec collect_backrefs n out =
  if n.kind = Backref then Array.iter (Buffer_int.add out) n.groups;
  Array.iter (fun s -> collect_backrefs s out) n.subs

(* Whether small counted repetitions are written out. The tests of the loops' needs and memo points turn it off. *)
let unrolling = ref true

let flatten terms =
  Array.of_list (List.concat_map (fun t -> if t.kind = Cat then Array.to_list t.subs else [ t ]) terms)

(* Writes out a counted repetition (see simplify): X{2,4} is X X (X (X)?)?, the optional copies one inside the other, so
   that a copy is tried only when the one before it matched. *)
let unroll n =
  let body = n.subs.(0) in
  let terms = List.init n.min (fun _ -> body) in
  let terms =
    if n.max > n.min then begin
      let optional = ref None in
      for _ = n.min to n.max - 1 do
        let inner = match !optional with None -> body | Some o -> with_subs Cat [| body; o |] in
        optional := Some { (with_subs Repeat [| inner |]) with min = 0; max = 1; lazy_ = n.lazy_ }
      done;
      match !optional with Some o -> terms @ [ o ] | None -> terms
    end
    else terms
  in
  match terms with [ t ] -> t | _ -> with_subs Cat (flatten terms)

(* Returns the tree with two rewrites, both of which V8 makes. Neither changes what the pattern matches or what any
   group holds. Without them a pattern can take time exponential in its counts for no reason that ECMA-262 gives.

   A backreference to a group it is inside of is the empty string. The group is not set until it closes, and the
   repetition around a group clears it at the start of each iteration, so inside the group it is never set.

   A repetition whose body cannot consume a character, such as (?:\b|(?=a)|){4,5}, is the body once when its minimum is
   one or more, and nothing when its minimum is zero. Every iteration of such a body is empty. ECMA-262 refuses an empty
   iteration after the minimum, so the iterations are exactly the minimum. Each of them starts with the groups of the
   body cleared and at the same position, so what the last one can do does not depend on the ones before it. Run as
   written, the matcher would try every combination of the ways through every iteration. *)
let rec simplify n open_ =
  if n.kind = Backref then if Array.for_all (fun g -> List.mem g open_) n.groups then node Empty else n
  else if Array.length n.subs = 0 then n
  else begin
    let opened = n.kind = Group && n.group > 0 in
    let inner = if opened then n.group :: open_ else open_ in
    let n = { n with subs = Array.map (fun s -> simplify s inner) n.subs } in
    let n =
      if n.kind = Cat && Array.exists (fun s -> s.kind = Cat) n.subs then
        (* A sequence inside a sequence (a written-out repetition) is the same sequence: flat, so that what reads the
           terms (the searches, the first characters) sees them all. *)
        { n with subs = flatten (Array.to_list n.subs) }
      else n
    in
    if n.kind = Repeat && n.max <> 0 && not (consumes n.subs.(0)) then if n.min = 0 then node Empty else n.subs.(0)
    else if
      !unrolling
      && n.kind = Repeat
      && n.min = n.max
      && n.max >= 2
      && n.max <= 32
      && (n.subs.(0).kind = Set || n.subs.(0).kind = Char)
    then
      (* An exact count of one character or class is that many of it in a row: a straight run of tests without a
         loop's counting, which pays where a search tries the run at every occurrence of a literal after it
         ([a-q][^u-z]{13}x). *)
      unroll n
    else if
      !unrolling
      && n.kind = Repeat
      && n.max >= 1
      && n.max <= 4
      && n.min >= 1
      && (not (nullable n.subs.(0)))
      && (not (has_capture n.subs.(0)))
      && size n.subs.(0) <= 32
      && (n.subs.(0).kind = Group || n.subs.(0).kind = Alt || n.subs.(0).kind = Cat)
    then
      (* A small counted repetition of a body that holds no group and cannot be empty is the body written out, with the
         copies beyond the least count optional one inside the other: the same matches in the same order, without a
         loop's counting. A body of one character stays a counted loop, which the matcher runs without choices. *)
      unroll n
    else n
  end

let add c i =
  if c.count = Array.length c.cinsts then begin
    let bigger = Array.make (max 16 (2 * c.count)) (inst Fail) in
    Array.blit c.cinsts 0 bigger 0 c.count;
    c.cinsts <- bigger
  end;
  c.cinsts.(c.count) <- i;
  c.count <- c.count + 1;
  c.count - 1

let pc c = c.count
let at c k = c.cinsts.(k)

(* The number of a new memo point for an instruction written here, or 0 when a choice here cannot be remembered. *)
let point c =
  if (not c.memo) || List.length c.open_ > max_memo_loops then 0
  else if List.exists (fun l -> not l.simple) c.open_ then 0
  else begin
    let loops = Array.make max_memo_loops (-1) and mins = Array.make max_memo_loops 0 in
    List.iteri
      (fun k l ->
        loops.(k) <- l.loop_;
        mins.(k) <- l.lmin)
      c.open_;
    c.cpoints <- { loops; mins } :: c.cpoints;
    c.npoints <- c.npoints + 1;
    c.npoints
  end

(* The first and last slot of the tracked groups inside n, or None when it has none. *)
let group_range c n =
  let range = ref None in
  let rec walk n =
    if n.kind = Group && n.group > 0 then begin
      match Hashtbl.find_opt c.tracked n.group with
      | Some t -> range := Some (match !range with Some (lo, hi) -> (min lo t, max hi t) | None -> (t, t))
      | None -> ()
    end;
    Array.iter walk n.subs
  in
  walk n;
  !range

(* The number of bytes of UTF-8 a code point takes. *)
let rune_len r = if r <= 0x7F then 1 else if r <= 0x7FF then 2 else if r <= 0xFFFF then 3 else 4

(* The need of an instruction from which no way leads to a match. Every need is capped at it. *)
let no_match = 1 lsl 30

(* count times need, capped at no_match. *)
let times_need count need =
  if count = 0 || need = 0 then 0 else if need >= no_match || count >= no_match / need then no_match else count * need

(* The least number of units a match of n consumes, capped at no_match: bytes of UTF-8 with utf8, and UTF-16 code units
   without it. Like the needs, it counts a character as its units when the matcher compares units for it and as one
   unit otherwise, so it can only be too small. *)
let rec min_length n utf8 =
  match n.kind with
  | Char -> if not (literal_rune n.r) then 1 else if utf8 then rune_len n.r else if n.r > 0xFFFF then 2 else 1
  | Set -> if Charset.is_empty (class_of n) then no_match else 1
  | Cat ->
      let total = ref 0 in
      Array.iter (fun s -> if !total < no_match then total := min no_match (!total + min_length s utf8)) n.subs;
      !total
  | Alt -> min no_match (Array.fold_left (fun acc s -> min acc (min_length s utf8)) no_match n.subs)
  | Group -> min_length n.subs.(0) utf8
  | Repeat -> times_need n.min (min_length n.subs.(0) utf8)
  | _ -> 0

let utf8_of_rune b r =
  if r < 0x80 then Buffer.add_char b (Char.chr r)
  else if r < 0x800 then begin
    Buffer.add_char b (Char.chr (0xC0 lor (r lsr 6)));
    Buffer.add_char b (Char.chr (0x80 lor (r land 0x3F)))
  end
  else if r < 0x10000 then begin
    Buffer.add_char b (Char.chr (0xE0 lor (r lsr 12)));
    Buffer.add_char b (Char.chr (0x80 lor ((r lsr 6) land 0x3F)));
    Buffer.add_char b (Char.chr (0x80 lor (r land 0x3F)))
  end
  else begin
    Buffer.add_char b (Char.chr (0xF0 lor (r lsr 18)));
    Buffer.add_char b (Char.chr (0x80 lor ((r lsr 12) land 0x3F)));
    Buffer.add_char b (Char.chr (0x80 lor ((r lsr 6) land 0x3F)));
    Buffer.add_char b (Char.chr (0x80 lor (r land 0x3F)))
  end

let utf16_of_rune b r =
  let unit u =
    Buffer.add_char b (Char.chr (u land 0xFF));
    Buffer.add_char b (Char.chr (u lsr 8))
  in
  if r > 0xFFFF then begin
    unit (Units.high_surrogate r);
    unit (Units.low_surrogate r)
  end
  else unit r

let split c =
  let m = point c in
  add c { (inst Split) with m }

(* Completes a split in the order the quantifier prefers. *)
let set_split c lazy_ at_ enter exit =
  let i = at c at_ in
  if lazy_ then begin
    i.x <- exit;
    i.y <- enter
  end
  else begin
    i.x <- enter;
    i.y <- exit
  end

(* Writes the instructions for n. With back set the instructions match right to left, as the body of a lookbehind
   must. *)
let rec emit c n back =
  match n.kind with
  | Empty -> ()
  | Char -> ignore (add c { (inst (if back then Char_back else Char_)) with r = n.r })
  | Set ->
      let set = class_of n in
      if Charset.is_empty set then ignore (add c (inst Fail))
      else ignore (add c { (inst (if back then Set_back else Set_)) with set = Some set })
  | Cat ->
      if back then
        for k = Array.length n.subs - 1 downto 0 do
          emit c n.subs.(k) true
        done
      else begin
        let i = ref 0 and len = Array.length n.subs in
        while !i < len do
          (* A run of literal characters becomes one comparison of units. *)
          let j = ref !i and lit = Buffer.create 8 and lit16 = Buffer.create 16 in
          while !j < len && n.subs.(!j).kind = Char && literal_rune n.subs.(!j).r do
            utf8_of_rune lit n.subs.(!j).r;
            utf16_of_rune lit16 n.subs.(!j).r;
            incr j
          done;
          if !j - !i >= 2 then begin
            ignore (add c { (inst Lit) with lit = Buffer.contents lit; lit16 = Buffer.contents lit16 });
            i := !j
          end
          else begin
            emit c n.subs.(!i) false;
            incr i
          end
        done
      end
  | Alt ->
      let jumps = ref [] and len = Array.length n.subs in
      (try
         Array.iteri
           (fun k sub ->
             if k = len - 1 then begin
               emit c sub back;
               raise Exit
             end;
             let m = point c in
             let s = add c { (inst Split) with m } in
             (at c s).x <- pc c;
             emit c sub back;
             jumps := add c (inst Jmp) :: !jumps;
             (at c s).y <- pc c)
           n.subs
       with Exit -> ());
      List.iter (fun j -> (at c j).x <- pc c) !jumps
  | Group -> (
      let tracked = if n.group = 0 then None else Hashtbl.find_opt c.tracked n.group in
      match tracked with
      | None -> emit c n.subs.(0) back
      | Some t ->
          ignore (add c { (inst Save_tmp) with a = t });
          emit c n.subs.(0) back;
          ignore (add c { (inst Capture) with a = t; flag = back }))
  | Bol -> ignore (add c (inst Bol_))
  | Eol -> ignore (add c (inst Eol_))
  | Line_start -> ignore (add c (inst Line_start_))
  | Line_end -> ignore (add c (inst Line_end_))
  | Word_boundary -> ignore (add c (inst (if n.fold then Word_boundary_fold else Word_boundary_)))
  | Not_word_boundary -> ignore (add c (inst (if n.fold then Not_word_boundary_fold else Not_word_boundary_)))
  | Backref ->
      let op =
        match (n.fold, back, c.cunicode) with
        | true, true, _ -> Backref_fold_back
        | true, false, _ -> Backref_fold
        | false, true, false -> Backref_units_back
        | false, true, true -> Backref_back
        | false, false, false -> Backref_units
        | false, false, true -> Backref_
      in
      (* When several groups share the name, at most one has taken part, and the others match the empty string. So
         matching each in turn matches the one that took part. *)
      Array.iter
        (fun g -> ignore (add c { (inst op) with a = Hashtbl.find c.tracked g; b = n.fold_version; flag = c.cunicode }))
        n.groups
  | Look ->
      let look = add c { (inst Look_) with flag = n.negative } in
      (* Whether the body matches depends on nothing outside it. *)
      let outer = c.open_ in
      c.open_ <- [];
      emit c n.subs.(0) n.behind;
      c.open_ <- outer;
      ignore (add c (inst (if n.negative then Look_fail else Look_ok)));
      (at c look).x <- pc c
  | Repeat -> emit_repeat c n back

and emit_body c body back clear =
  (match clear with Some (lo, hi) -> ignore (add c { (inst Clear) with a = lo; b = hi }) | None -> ());
  emit c body back

and emit_repeat c n back =
  let body = n.subs.(0) in
  if n.max <> 0 then begin
    let min = n.min and max = n.max in
    if (not back) && (body.kind = Char || (body.kind = Set && not (Charset.is_empty (class_of body)))) then begin
      let m = point c in
      ignore (add c { (inst (if n.lazy_ then Rep_lazy else Rep)) with r = body.r; set = body.set; min; max; m })
    end
    else begin
      let clear = group_range c body in
      (* A body that always consumes something needs no check for empty iterations, so the unbounded forms and the
         optional form are plain choices. *)
      let plain = not (nullable body) in
      if plain && n.min = 0 && n.max = unbounded then begin
        let at_ = split c in
        let enter = pc c in
        emit_body c body back clear;
        ignore (add c { (inst Jmp) with x = at_ });
        let exit = pc c in
        set_split c n.lazy_ at_ enter exit
      end
      else if plain && n.min = 1 && n.max = unbounded then begin
        let enter = pc c in
        emit_body c body back clear;
        let at_ = split c in
        let exit = pc c in
        set_split c n.lazy_ at_ enter exit
      end
      else if plain && n.min = 0 && n.max = 1 then begin
        let at_ = split c in
        let enter = pc c in
        emit_body c body back clear;
        let exit = pc c in
        set_split c n.lazy_ at_ enter exit
      end
      else begin
        let k = c.cloops in
        c.cloops <- c.cloops + 1;
        ignore (add c { (inst Loop_init) with a = k });
        let iter, iter16 = if back then (0, 0) else (min_length body true, min_length body false) in
        let simple = max = unbounded && min <= 1 in
        (* The loop is left or entered from here, and what follows either depends on the loops around this one. *)
        let m = if simple then point c else 0 in
        let loop_ = add c { (inst Loop) with a = k; min; max; flag = n.lazy_; iter; iter16; m } in
        ignore (add c { (inst Loop_iter) with a = k });
        c.open_ <- c.open_ @ [ { loop_ = k; lmin = min; simple } ];
        emit_body c body back clear;
        c.open_ <- List.filteri (fun i _ -> i < List.length c.open_ - 1) c.open_;
        ignore (add c { (inst Jmp) with x = loop_ });
        (at c loop_).x <- pc c
      end
    end
  end

(* Works out, for every instruction, the least number of units of text that any way from it to a match consumes going
   forward, and writes it into the counted loops. It returns the need of the whole program and the number of passes
   it made. The matcher gives up at a counted loop when fewer units than that are left, as V8 does at every choice.

   A counted loop that is entered needs its least count of iterations, each of them the least its body consumes
   (iter), and then what follows it. The loop instruction itself is also reached after each iteration, so its own need
   counts only what follows, and the matcher adds the iterations it has still to make. A character is counted as one
   unit, a literal run as its units and a single character loop as its minimum, so the figure can only be too small.
   The body of a lookaround ends at its own last instruction and the instruction that opens one goes straight to what
   follows it, so a body is counted by itself. An instruction that matches right to left counts nothing. *)
let needs_in (insts : inst array) utf8 =
  let len = Array.length insts in
  let need = Array.make len no_match in
  let passes = ref 0 and changed = ref true in
  (* The needs only fall, and nearly every way goes forward, so going through the instructions from the last to the
     first settles all but the loops in one pass. *)
  while !changed do
    changed := false;
    incr passes;
    for pc = len - 1 downto 0 do
      let i = insts.(pc) in
      let n = ref no_match in
      let next to_ weight =
        let v = need.(to_) + weight in
        if v < !n then n := v
      in
      let after = pc + 1 in
      (match i.op with
      | Match | Look_ok | Look_fail -> n := 0
      | Fail -> ()
      | Char_ | Set_ -> next after 1
      | Lit -> next after (if utf8 then String.length i.lit else String.length i.lit16 / 2)
      | Rep | Rep_lazy -> next after i.min
      | Jmp ->
          (* The one way that goes back is the jump at the end of the body of a loop, to the instruction at its head,
             which this pass has not reached. The need of that instruction is the need of what follows the loop, since
             its other way, through the body, comes back to it and cannot need less. So the jump takes what follows the
             loop, which this pass has settled. *)
          let head = insts.(i.x) in
          if i.x < pc && head.op = Loop then next head.x 0
          else if i.x < pc && head.op = Split then begin
            next head.x 0;
            next head.y 0
          end
          else next i.x 0
      | Look_ -> next i.x 0
      | Split ->
          next i.x 0;
          next i.y 0
      | Loop ->
          next after 0;
          next i.x 0
      | Loop_init ->
          let l = insts.(pc + 1) in
          let iter = if utf8 then l.iter else l.iter16 in
          next after (times_need l.min iter)
      | _ -> next after 0);
      let n = min !n no_match in
      if n < need.(pc) then begin
        need.(pc) <- n;
        changed := true
      end
    done
  done;
  Array.iteri (fun pc i -> if i.op = Loop then if utf8 then i.need <- need.(pc) else i.need16 <- need.(pc)) insts;
  ((if len = 0 then 0 else need.(0)), !passes)

(* set_needs in both units: the needs of the program in bytes of UTF-8 and in UTF-16 code units. *)
let set_needs insts =
  let need16 = fst (needs_in insts false) in
  (fst (needs_in insts true), need16)

(* Works out the quick of every instruction, and the number of passes it made. Every figure is a set that can only be
   too large: an assertion is taken to hold, a backreference and an instruction that matches right to left to allow
   anything. A positive lookahead needs both its body and what follows it to go on from the same position, so it allows
   what both allow. *)
let quicks (insts : inst array) =
  let len = Array.length insts in
  let q = Array.make len no_quick in
  let passes = ref 0 and changed = ref true in
  while !changed do
    changed := false;
    incr passes;
    for pc = len - 1 downto 0 do
      let i = insts.(pc) in
      let n =
        match i.op with
        | Match | Look_ok | Look_fail -> all_quick
        | Fail -> no_quick
        | Char_ -> quick_add_rune no_quick i.r
        | Set_ -> quick_add_set no_quick (Option.get i.set)
        | Lit -> quick_add_rune no_quick (Char.code i.lit.[0])
        | Rep | Rep_lazy ->
            let n = match i.set with Some s -> quick_add_set no_quick s | None -> quick_add_rune no_quick i.r in
            if i.min = 0 then quick_union n q.(pc + 1) else n
        | Jmp ->
            (* The jump at the end of the body of a loop goes back to the head of the loop, which this pass has not
               reached. The head allows what follows the loop, which this pass has settled, and what the body starts
               with. So the jump takes what follows the loop too, and the passes settle however deep the loops nest. *)
            let n = q.(i.x) and head = insts.(i.x) in
            if i.x < pc && head.op = Loop then quick_union n q.(head.x)
            else if i.x < pc && head.op = Split then quick_union (quick_union n q.(head.x)) q.(head.y)
            else n
        | Split -> quick_union q.(i.x) q.(i.y)
        | Loop -> quick_union q.(pc + 1) q.(i.x)
        | Eol_ -> { no_quick with end_ = q.(pc + 1).end_ }
        | Look_ ->
            let n = q.(i.x) in
            if not i.flag then begin
              let b = q.(pc + 1) in
              {
                q0 = n.q0 land b.q0;
                q1 = n.q1 land b.q1;
                q2 = n.q2 land b.q2;
                q3 = n.q3 land b.q3;
                other = n.other && b.other;
                mid = n.mid && b.mid;
                end_ = n.end_ && b.end_;
              }
            end
            else n
        | Char_back | Set_back | Backref_ | Backref_back | Backref_fold | Backref_fold_back | Backref_units
        | Backref_units_back ->
            all_quick
        | _ -> q.(pc + 1)
      in
      if n <> q.(pc) then begin
        q.(pc) <- n;
        changed := true
      end
    done
  done;
  (q, !passes)

let set_quick insts = fst (quicks insts)

(* Gives each greedy single-character loop the ASCII bytes what follows it can start with, when there are one to three
   and nothing else can follow. *)
let set_back (insts : inst array) (quick : quick array) =
  Array.iteri
    (fun pc i ->
      if i.op = Rep && pc + 1 < Array.length quick then begin
        let q = quick.(pc + 1) in
        if not (q.other || q.mid) then begin
          let b = Buffer.create 4 in
          for c = 0 to 127 do
            if quick_has q c then Buffer.add_char b (Char.chr c)
          done;
          if Buffer.length b >= 1 && Buffer.length b <= 3 then i.back <- Buffer.contents b
        end
      end)
    insts

(* Whether every way through n begins with ^. *)
let rec starts_at_bol n =
  match n.kind with
  | Bol -> true
  | Cat -> starts_at_bol n.subs.(0)
  | Alt -> Array.for_all starts_at_bol n.subs
  | Group -> starts_at_bol n.subs.(0)
  | Repeat -> n.min >= 1 && starts_at_bol n.subs.(0)
  | _ -> false

exception Unknown

(* Appends, as inclusive pairs, the code points a match of n can start with. It returns whether n can match the empty
   string, and raises Unknown when the set is not known. Assertions are treated as matching the empty string, which
   can only make the set larger than it need be. *)
let rec first_chars n pairs =
  match n.kind with
  | Char ->
      Buffer_int.add pairs n.r;
      Buffer_int.add pairs n.r;
      false
  | Set ->
      Buffer_int.append_array pairs (class_of n).ranges;
      false
  | Cat ->
      let len = Array.length n.subs in
      let rec go k = if k = len then true else if not (first_chars n.subs.(k) pairs) then false else go (k + 1) in
      go 0
  | Alt -> Array.fold_left (fun empty s -> first_chars s pairs || empty) false n.subs
  | Group -> first_chars n.subs.(0) pairs
  | Repeat -> if n.max = 0 then true else first_chars n.subs.(0) pairs || n.min = 0
  | Backref -> raise Unknown
  | _ -> true

(* Whether a tree nests fewer than limit levels. *)
let rec shallower_than n limit = limit > 0 && Array.for_all (fun s -> shallower_than s (limit - 1)) n.subs

(* Compiles a tree. With groups the program records every one of that many capturing groups, group g in slot g - 1, so
   that a match can report what each captured; without it, only the groups some backreference names. It returns the
   program and the simplified tree, which the searches and the automata read. *)
let compile_program root unicode groups =
  let root = simplify root [] in
  let c =
    {
      cinsts = [||];
      count = 0;
      tracked = Hashtbl.create 16;
      cloops = 0;
      cunicode = unicode;
      memo = false;
      open_ = [];
      cpoints = [];
      npoints = 0;
    }
  in
  let referenced = Buffer_int.create 4 in
  collect_backrefs root referenced;
  (* A choice can be remembered to have failed only where what a group holds cannot change what follows. *)
  c.memo <- Buffer_int.length referenced = 0;
  let backrefs = Buffer_int.length referenced > 0 in
  (match groups with
  | Some n ->
      for g = 1 to n do
        Hashtbl.replace c.tracked g (g - 1)
      done
  | None ->
      (* Slots follow group order, so the groups inside any one subtree take a contiguous range of slots. *)
      let refs = List.sort_uniq compare (Array.to_list (Buffer_int.contents referenced)) in
      List.iteri (fun slot g -> Hashtbl.replace c.tracked g slot) refs);
  emit c root false;
  ignore (add c (inst Match));
  let insts = Array.sub c.cinsts 0 c.count in
  let need, need16 = set_needs insts in
  let quick = set_quick insts in
  set_back insts quick;
  let anchored = starts_at_bol root in
  let first =
    if anchored then None
    else begin
      let pairs = Buffer_int.create 16 in
      match first_chars root pairs with
      | false -> Some (Charset.make (Buffer_int.contents pairs))
      | true -> None
      | exception Unknown -> None
    end
  in
  ( {
      insts;
      groups = Hashtbl.length c.tracked;
      loops = c.cloops;
      units = not unicode;
      anchored;
      need;
      need16;
      points = Array.of_list (List.rev c.cpoints);
      quick;
      first;
    },
    root,
    backrefs )
