(* A value computed when it is first asked for and kept, as Rust's LazyLock is used in the crate. OCaml's own lazy
   values may not be forced from two domains at once (OCaml 5 raises Lazy.Undefined), so this keeps the value in an
   Atomic: two that ask at once may both compute it, and both then take the one kept. That is sound on OCaml 4.14 with
   system threads and on OCaml 5 with domains, and the value must not depend on which computation is kept. *)

type 'a t = { compute : unit -> 'a; cell : 'a option Atomic.t }

let make compute = { compute; cell = Atomic.make None }

let get o =
  match Atomic.get o.cell with
  | Some v -> v
  | None -> (
      let v = o.compute () in
      ignore (Atomic.compare_and_set o.cell None (Some v));
      match Atomic.get o.cell with Some v -> v | None -> v)
