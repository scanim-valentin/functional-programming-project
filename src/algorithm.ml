open Graph
open Tools
open Dijkstra
(*Initializes the flow for every arc*)
let init_ff gr_int = gmap gr_int ( fun lbl->(0,lbl) ) (*gr : flow graph*)
let add_double_arc gr_int n1 n2 (lbl1,lbl2) = 
  let gr_r = add_arc gr_int n1 n2 lbl1 in
  add_arc gr_r n2 n1 lbl2 
  
(*Returns the gap graph*)

(* val e_fold: 'a graph -> ('b -> id -> id -> 'a -> 'b) -> 'b -> 'b *)
(*             (src)        (acc) (n1)  (n2)  (data arc)   (acc)  *)
let gr_gap gr_flow = e_fold gr_flow add_double_arc (clone_nodes gr_flow)
    

  (*gr_gap : int graph*)  
(*
let rec research gr idA idB =   
  (*Execution*)
  
  match (find_arc gr idA idB) with 
    |
    |

let ford_fulkerson gr_int id1 id2 = *)
