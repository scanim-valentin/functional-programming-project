(* Yes, we have to repeat open Graph. *)
open Graph

(* assert false is of type ∀α.α, so the type-checker is happy. *)
let clone_nodes gr = n_fold gr new_node empty_graph 

(*On en est là lol*)
let gmap gr f = 
  let gmap_aux gr id1 id2 ngr = new_arc gr id1 id2 (f ngr) in 
  e_fold gr gmap_aux (clone_nodes gr) 

let add_arc gr n1 n2 p =
  match find_arc gr n1 n2 with
  | Some x -> new_arc gr n1 n2 (x + p)
  | None -> new_arc gr n1 n2 p

let string_of_flow (a,b) = ((string_of_int a)^"/")^(string_of_int b)
