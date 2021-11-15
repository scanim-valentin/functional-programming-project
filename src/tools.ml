(* Yes, we have to repeat open Graph. *)
open Graph

(* assert false is of type ∀α.α, so the type-checker is happy. *)
let clone_nodes gr = n_fold gr new_node empty_graph 

(*On en est là lol*)
let gmap gr f = 
  let rec gmap_aux gr acc f =  
  in 
  e_fold gr gmap_aux gr (clone_nodes gr) 

let add_arc gr n1 n2 p = assert false
