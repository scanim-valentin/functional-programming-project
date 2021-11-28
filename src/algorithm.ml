open Graph
open Tools
(*Initializes the flow for every arc*)
let init_ff gr_int = gmap gr_int ( fun lbl->(0,lbl) ) 
let add_double_arc gr_int n1 n2 (lbl1,lbl2) = 
  let gr_r = add_arc gr_int n1 n2 lbl1 in
  add_arc gr_r n2 n1 lbl2 
  
(*Returns the gap graph*)

(* val e_fold: 'a graph -> ('b -> id -> id -> 'a -> 'b) -> 'b -> 'b *)
(*             (src)        (acc) (n1)  (n2)  (data arc)   (acc)  *)
let gap_from_flow gr_flow = e_fold gr_flow add_double_arc (clone_nodes gr_flow)

let ford_fulkerson gr_flow id1 id2 = 
    (*let gr_gap = gap_from_flow gr_flow in (*Initialized gap graph*)
    *)
    (*Returns the first out arc to a non marked node or none if all have been marked*)
    let rec first_non_marked out_arc_lst marked_nodes_lst =
        let cond_arc (idA,_) = match (List.find_opt ( fun idB -> (idB == idA) ) marked_nodes_lst ) with
            |None -> false
            |Some id -> true in
        List.find_opt cond_arc out_arc_lst in
        
    (*Returns flow variation for a path*)
    (*TODO*)
    let rec flow_variation path = match path with 
     |[] -> 0
     |h::tl -> 0 in
    
    let rec iter acc_path marked_nodes = match acc_path with
        |[] -> (*beginning depth path search from id1 to id2*)
           ( match (out_arcs gr_flow id1) with
                |[]                -> (*id1 is isolated*)
                    gr_flow
                |out_arcs_list -> (*iterating on the first node that is not 
                                in the marked_nodes list and marking the next node*)
                    ( match (first_non_marked out_arcs_list marked_nodes ) with
                        |None -> (*All next nodes have been marked (i.e. all path have been explored)*)
                            gr_flow (*Algorithm ends here*)
                        |Some (id_next,_) -> (*Iterating on the next node*)
                            iter [id_next] (id_next::marked_nodes) ))
                            
        |h::tl -> (*is path complete?*) 
            if (h == id2) then (*path is complete*) 
                (*do stuff*)
                gr_flow
            else (*destination hasn't been reached yet*)
                ( match (out_arcs gr_flow h) with
                |[]                -> (*h is isolated*)
                    gr_flow
                |out_arcs_list -> (*iterating on the first node that is not 
                                in the marked_nodes list and marking the next node*)
                     match (first_non_marked out_arcs_list marked_nodes ) with
                        |None -> (*All next nodes have been marked (i.e. all path have been explored)*)
                            gr_flow (*Algorithm ends here*)
                        |Some (id_next,_) -> (*Iterating on the next node*)
                            iter [id_next] (id_next::marked_nodes) )
        in
        
        iter [] []
        
