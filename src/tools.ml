(* Yes, we have to repeat open Graph. *)
open Graph

(* assert false is of type ∀α.α, so the type-checker is happy. *)
let clone_nodes (gr : a' graph) = n_fold gr new_node empty_graph 

let gmap gr f = assert false


