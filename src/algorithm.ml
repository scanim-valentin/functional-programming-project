open Graph
open Tools

let ford_fulkerson gr_int id1 id2 = 
  (*Initializing the flow*)
  let init_ff gr = gmap gr_int ( fun lbl->(0,lbl) ) in (*gr : flow graph*)
  
  (*Execution*)
  let gr_gap = e_fold (clone_nodes gr_int) ( fun gr n1 n2 (lbl1,lbl2) -> ( add_arc gr n1 n2 lbl1 ) ; ( add_arc gr n2 n1 lbl2 ) ) in 
  (*gr_gap : int graph*)
  