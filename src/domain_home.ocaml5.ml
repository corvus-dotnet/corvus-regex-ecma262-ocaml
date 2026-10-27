(* The working memory each domain keeps for the matches it makes, in OCaml 5's domain-local storage (OCaml 4.14 runs one
   domain, and domain_home.ocaml4.ml keeps none). The slot holds the memory while no match of the domain holds it: a
   thread of the domain that finds it held takes the pattern's, or makes its own. dune copies this file to
   domain_home.ml on OCaml 5. *)

type slot = { home : Vm_state.t option; held : Vm_state.t option Atomic.t }

let key =
  Domain.DLS.new_key (fun () ->
      let home = Some (Vm_state.create ()) in
      { home; held = Atomic.make home })

(* The domain's working memory, or None when a match of the domain holds it. *)
let take () =
  let s = Domain.DLS.get key in
  if Atomic.compare_and_set s.held s.home None then s.home else None

(* Gives the domain's working memory back, when st is it. *)
let give_back st =
  let s = Domain.DLS.get key in
  match s.home with Some home when home == st -> Atomic.set s.held s.home | _ -> ()
