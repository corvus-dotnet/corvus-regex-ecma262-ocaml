(* The working memory of the backtracking matcher, shared by its two encodings (vm.rs of the Rust crate). *)

(* The number of steps the matcher takes between two looks at the clock and at the depth of its stack. A step is one
   instruction, and eight characters of a single character loop or of a backreference. A step takes a few
   nanoseconds, so the clock is read every ten to twenty microseconds, a match that ends before its first check never
   reads it, and a match goes past its limit by that much at most. *)
let check_every = 1 lsl 12

(* The number of entries the backtracking stack of a match with a limit may hold. A match that needs more is
   abandoned. A pattern that keeps a choice for every character of its text needs one entry or a few for each, so this
   is room for a text of some hundred thousand characters, and a pattern that pushes without consuming, such as
   (?:a?){2147483647}, reaches it in a few milliseconds. It is looked at with the clock, so the stack can go past it by
   check_every entries. *)
let max_stack = 1 lsl 20

(* Why a match was abandoned. *)
let stop_none = 0
let stop_time = 1
let stop_memory = 2

(* The start match_with gives for no match, and for a match abandoned at its limit of time or of memory: no position
   of a text is any of them. *)
let no_match = -1
let stopped_time = -2
let stopped_memory = -3

(* The kinds of entry on the backtracking stack. A choice entry says where to resume. An undo entry restores a value
   that was overwritten after the entries below it were pushed. *)

(* Resumes at pc with the position pos. *)
let e_choice = 0

(* Belongs to a greedy single character loop that stopped at pos. Resuming gives back one character, down to the
   position a. *)
let e_rep = 1

(* Belongs to a lazy single character loop that has taken a characters and stands at pos. Resuming takes one more. *)
let e_rep_lazy = 2

(* e_look marks the start of a positive lookaround and e_look_neg of a negative one. Both hold the position to restore,
   and e_look_neg the instruction to continue at when its body has failed. *)
let e_look = 3
let e_look_neg = 4

(* Lies under the choices of a memo point. When it is reached, every way from the point has failed, and the bit pos of
   the memory is set. *)
let e_mark = 5

(* Restores the entry position of group pc to pos. *)
let e_tmp = 6

(* Restores the start and end of group pc to pos and a. *)
let e_cap = 7

(* Restores the count and the iteration start of loop pc to pos and a. *)
let e_counter = 8

(* The value of a group that has not taken part, and of the start of an iteration that has not started. *)
let none = -1

(* The largest memory of failed places, in bytes: 8 MB. A match that would need more goes without it. *)
let max_memo_bytes = 1 lsl 23

(* The number of steps for each position of the text that a match makes before it takes the memory into use. *)
let memo_steps_per_position = 64

(* The largest memory kept between matches, in bytes. *)
let max_retained_memo = 1 lsl 19

(* The depth of the backtracking stack beyond which a memo point is not marked. A mark is an entry of the stack, and a
   pattern that keeps no choice for a character still marks a place for each, so on a long text the marks alone would
   reach max_stack. A place that is not marked is not remembered, which loses time and nothing else. *)
let max_mark_depth = max_stack / 2

(* The largest stack kept between matches, in entries. *)
let max_retained_stack = 1 lsl 16

(* The monotonic clock, in nanoseconds. *)
external now_ns : unit -> (int[@untagged]) = "cre_now_ns_byte" "cre_now_ns" [@@noalloc]

(* Makes every match take its memory of failed places before its first step. CI sets the variable for a second run of
   the tests and of the test data, so that every pattern runs with the memory in use. *)
let memo_from_the_start = match Sys.getenv_opt "CORVUS_REGEX_MEMO_FROM_THE_START" with Some "1" -> true | _ -> false

(* The working memory of one match. A match allocates only while the stack is still growing to the depth the pattern
   and the text need. *)
type t = {
  (* The backtracking stack: four ints an entry (its kind, pc, pos and a), sp entries in use. *)
  mutable stack : int array;
  mutable sp : int;
  (* caps holds the start and end of each tracked group, or none, and tmp the position at which each was entered. *)
  mutable caps : int array;
  mutable tmp : int array;
  (* For each loop, the number of iterations started and the position at which the last one started. *)
  mutable counts : int array;
  (* Where the match that was found ends. *)
  mutable end_ : int;
  (* The time the match may take in nanoseconds, or 0 for no limit. The clock starts at the first check, so deadline is
     set (above -1) when the first check has been made. *)
  mutable limit : int;
  mutable deadline : int;
  (* The number of steps left before the next check, and why the match was abandoned. *)
  mutable steps : int;
  mutable stop : int;
  (* The memory of the places where a way has failed: one bit for each memo point of the program, each position of the
     text and each state of the loops around the point. A bit is set when every way from the place has failed, and a
     way that comes to a place whose bit is set fails at once. So no place is gone through twice, and a pattern of
     repetitions inside repetitions takes time proportional to its size times the text.

     The memory costs its size to clear, and a mark on the stack for each place gone through, so a match starts
     without it. It takes it at the check by which it has made as many steps as the memory has bits over 64 and
     memo_steps_per_position more for each position of the text, which a match that goes through its text once or a
     few times never makes. memo_on says the memory is in use, memo_off that it never will be (the program has no memo
     points, or the memory would be larger than max_memo_bytes), and width is the number of positions of the text. *)
  mutable memo : Bytes.t;
  mutable memo_on : bool;
  mutable memo_off : bool;
  mutable width : int;
  mutable checks : int;
  mutable points : int;
  (* Where an occurrence of the search's literals ends (out.(0)), a place an automaton reached (out.(1)), the starts an
     occurrence allows (out.(2) and out.(3), see starts_before in the matcher), and the buffer the packed search over
     UTF-16 packs units into, made on the first search that wants it. *)
  out : int array;
  mutable pack : Bytes.t;
}

let create () =
  {
    stack = Array.make 256 0;
    sp = 0;
    caps = [||];
    tmp = [||];
    counts = [||];
    end_ = 0;
    limit = 0;
    deadline = -1;
    steps = check_every;
    stop = stop_none;
    memo = Bytes.empty;
    memo_on = false;
    memo_off = true;
    width = 0;
    checks = 0;
    points = 0;
    out = [| 0; 0; 0; 0 |];
    pack = Bytes.empty;
  }

(* Takes the memory of failed places into use when the match has made enough steps to pay for it. *)
let remember st =
  let bits = (st.points * st.width) lsl Compile.max_memo_loops in
  let bytes = (bits + 7) lsr 3 in
  if bytes > max_memo_bytes then st.memo_off <- true
  else if st.checks * check_every >= (bytes / 8) + (memo_steps_per_position * st.width) || memo_from_the_start then begin
    if Bytes.length st.memo < bytes then st.memo <- Bytes.make bytes '\000' else Bytes.fill st.memo 0 bytes '\000';
    st.memo_on <- true
  end

(* Called every check_every steps. It reports whether the match may go on. *)
let check st depth =
  if (not st.memo_on) && not st.memo_off then begin
    st.checks <- st.checks + 1;
    remember st
  end;
  if st.limit = 0 then true
  else if depth > max_stack then begin
    st.stop <- stop_memory;
    false
  end
  else begin
    let now = now_ns () in
    if st.deadline < 0 then begin
      st.deadline <- now + st.limit;
      true
    end
    else if now > st.deadline then begin
      st.stop <- stop_time;
      false
    end
    else true
  end

let[@inline] memo_get st idx = Char.code (Bytes.unsafe_get st.memo (idx lsr 3)) land (1 lsl (idx land 7)) <> 0

let[@inline] memo_set st idx =
  let k = idx lsr 3 in
  Bytes.unsafe_set st.memo k (Char.unsafe_chr (Char.code (Bytes.unsafe_get st.memo k) lor (1 lsl (idx land 7))))

(* Pushes an entry, growing the stack when it is full. *)
let[@inline never] grow st =
  let bigger = Array.make (2 * Array.length st.stack) 0 in
  Array.blit st.stack 0 bigger 0 (4 * st.sp);
  st.stack <- bigger

let[@inline] push st kind pc pos a =
  let k = 4 * st.sp in
  if k + 4 > Array.length st.stack then grow st;
  let s = st.stack in
  Array.unsafe_set s k kind;
  Array.unsafe_set s (k + 1) pc;
  Array.unsafe_set s (k + 2) pos;
  Array.unsafe_set s (k + 3) a;
  st.sp <- st.sp + 1

(* Applies one undo entry, at index e of the stack. *)
let[@inline] undo st e =
  let s = st.stack in
  let k = s.((4 * e) + 1) in
  let kind = s.(4 * e) in
  if kind = e_tmp then st.tmp.(k) <- s.((4 * e) + 2)
  else if kind = e_cap then begin
    st.caps.(2 * k) <- s.((4 * e) + 2);
    st.caps.((2 * k) + 1) <- s.((4 * e) + 3)
  end
  else if kind = e_counter then begin
    st.counts.(2 * k) <- s.((4 * e) + 2);
    st.counts.((2 * k) + 1) <- s.((4 * e) + 3)
  end

(* The bit of the memory for the memo point m at a position, with the state of the loops around the point: for each,
   whether it would refuse an iteration that ended here as empty. *)
let memo_index (prog : Compile.program) st m pos =
  let pt = prog.points.(m - 1) in
  let idx = ref ((((m - 1) * st.width) + pos) lsl Compile.max_memo_loops) in
  for k = 0 to Compile.max_memo_loops - 1 do
    let l = pt.loops.(k) in
    if l >= 0 && st.counts.(2 * l) > pt.mins.(k) && st.counts.((2 * l) + 1) = pos then idx := !idx lor (1 lsl k)
  done;
  !idx

(* Readies the memory for a match of the program over a text of len units, with a limit in nanoseconds (0 for none). *)
let reset st (prog : Compile.program) len limit =
  let ng = 2 * prog.groups in
  if Array.length st.caps <> ng then st.caps <- Array.make ng none else Array.fill st.caps 0 ng none;
  if Array.length st.tmp <> prog.groups then st.tmp <- Array.make prog.groups 0;
  let nl = 2 * prog.loops in
  if Array.length st.counts <> nl then st.counts <- Array.make nl 0 else Array.fill st.counts 0 nl 0;
  st.limit <- limit;
  st.deadline <- -1;
  st.steps <- check_every;
  st.stop <- stop_none;
  st.memo_on <- false;
  st.memo_off <- Array.length prog.points = 0;
  st.checks <- 0;
  st.width <- len + 1;
  st.points <- Array.length prog.points;
  st.sp <- 0;
  if (not st.memo_off) && memo_from_the_start then remember st

(* Gives back what a match left larger than is kept between matches. *)
let trim st =
  if Array.length st.stack > 4 * max_retained_stack then st.stack <- Array.make 256 0;
  if Bytes.length st.memo > max_retained_memo then st.memo <- Bytes.empty
