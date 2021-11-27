open Graph
open Tools
open Dijkstra
(*Initializes the flow for every arc*)
let init_ff gr_int = gmap gr_int ( fun lbl->(0,lbl) ) 
let add_double_arc gr_int n1 n2 (lbl1,lbl2) = 
  let gr_r = add_arc gr_int n1 n2 lbl1 in
  add_arc gr_r n2 n1 lbl2 
  
(*Returns the gap graph*)

(* val e_fold: 'a graph -> ('b -> id -> id -> 'a -> 'b) -> 'b -> 'b *)
(*             (src)        (acc) (n1)  (n2)  (data arc)   (acc)  *)
let gap_from_flow gr_flow = e_fold gr_flow add_double_arc (clone_nodes gr_flow)

let ford-fulkerson gr_flow id1 id2 = 
    let gr_gap = gap_from_flow gr_flow in (*Initialized gap graph*)
    
    let is_marked mk_nds id = match (List.findopt mk_nds id) with
                        |None -> False
                        |Some _ -> True in
    
    let first_non_marked out_lst = match out_lst with
        |[] -> None
        |_::_ -> Some (List.find (fun (_,id_node) -> is_marked id_node) out_lst) in
    
    let rec iter acc_path marked_nodes = match acc_path with
        |[] -> (*beginning depth path search from id1 to id2*)
            match (out_arcs gr_flow id1) with
                |[]                -> (*id1 is isolated*)
                    gr_flow
                |out_arcs_list -> (*iterating on the first node that is not 
                                in the marked_nodes list and marking the next node*)
                     match (first_non_marked out_arcs_list) with
                        |None -> (*All next nodes have been marked (i.e. all path have been explored)*)
                            ()
                        |Some (_,id_next) -> (*Iterating on the next node*)
                            iter [](*TO BE CONTINUED*)
        |h::tl -> (*is path complete?*) 
            match h with
            
        
