(* OCaml 4.14 runs one domain, so it keeps no working memory of its own: a match takes the pattern's (see
   domain_home.ocaml5.ml). dune copies this file to domain_home.ml on OCaml 4.14. *)

let take () : Vm_state.t option = None
let give_back (_ : Vm_state.t) = ()
