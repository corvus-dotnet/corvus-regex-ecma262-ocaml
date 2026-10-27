(* A compiled program with what its search learned of it (the fields of the crate's Program beyond the matcher's): the
   search, the anchored automaton, the forward automaton and its skip window, and the states after a literal every match
   starts with. *)

type t = {
  p : Compile.program;
  (* How the search finds where a match can start (see Search). *)
  search : Search.t;
  (* The anchored automaton that finds where a match from a start ends, for a program that records no group and needs
     no backtracking (see Adfa). *)
  anchored_dfa : Adfa.anchored option;
  (* For a pattern whose search tries every start its first set allows, the forward automaton (see Adfa.build_forward),
     built on the first search that wants it. *)
  forward : Adfa.anchored option Once.t option;
  (* For the same patterns, the window of positions read one unit at a time before the first set is searched for (see
     Adfa.skip). *)
  skip : Adfa.skip option;
  (* For a pattern every match of which starts with one literal (a window at distance 0 of one literal over UTF-8), the
     anchored automaton's states after it (see Adfa.after) and its length: an occurrence goes on from there, its units
     not read again. *)
  after_literal : (int array * int) option;
}

(* Compiles a tree (see Compile.compile_program) and plans its search. *)
let make root unicode groups =
  let p, root, backrefs = Compile.compile_program root unicode groups in
  let anchored_dfa = if p.groups = 0 && not backrefs then Adfa.build root else None in
  if p.anchored then { p; search = Search.none; anchored_dfa; forward = None; skip = None; after_literal = None }
  else begin
    let search = Search.plan root backrefs p.first (not unicode) in
    let after_literal =
      match (search.plan, anchored_dfa) with
      | Search.Windows { literals; hi = 0; _ }, Some dfa when literals.single8 <> "" -> (
          match Adfa.after dfa literals.single8 with
          | Some rows -> Some (rows, String.length literals.single8)
          | None -> None)
      | _ -> None
    in
    let plain = match (search.plan, search.shape) with Search.Scan, Search.No_shape -> true | _ -> false in
    (* The tree is kept, and later built from, on the thread that searches: only a shallow one is kept. *)
    if (not !Search.no_search) && (not backrefs) && Compile.shallower_than root 64 && plain then
      {
        p;
        search;
        anchored_dfa;
        forward = Some (Once.make (fun () -> Adfa.build_forward root));
        skip = Adfa.skip_window root;
        after_literal;
      }
    else { p; search; anchored_dfa; forward = None; skip = None; after_literal }
  end
