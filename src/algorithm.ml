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

(*Returns the first out arc to a non marked node or none if all have been marked*)
let rec first_non_marked out_arc_lst marked_nodes_lst =
        let cond_arc (idA,_) = match (List.find_opt ( fun idB -> (idB == idA) ) marked_nodes_lst ) with
            |None -> false
            |Some id -> true in
        List.find_opt cond_arc out_arc_lst
        
(*Returns flow variation for a path*)
    (*flow_variation: (id*int) list -> int -> int *)
let rec flow_variation path min_acc = match path with 
     |[] -> 0 
     |(_,value)::tl -> (*Getting the minimal gap*)
        if( value < min_acc )then 
            flow_variation tl value 
        else 
            flow_variation tl min_acc 

(*cursed*)
let rec update_graph path gr_int mn = match path with
        |[] -> gr_int
        |(id1,val1)::((id2,val2)::tl) -> (*Updating the out and in arc values along the given path*)
            match ((find_arc gr_int id1 id2),(find_arc gr_int id2 id1)) with
                |(None,None) -> update_graph ((id2,val2)::tl) gr_int mn 
                |(None,Some x) ->update_graph ((id2,val2)::tl) (new_arc gr_int id2 id1 (x-mn)) mn ; 
                |(Some x,None) -> update_graph ((id2,val2)::tl) (new_arc gr_int id1 id2 (x+mn)) mn  ;
                |(Some x1,Some x2) -> update_graph ((id2,val2)::tl) (new_arc (new_arc gr_int id2 id1 (x2-mn)) id1 id2 (x1+mn)) mn  

(*Finds a path in a graph between id1 and id2 based on depth search*)
let rec find_path id1 id2 acc_path marked_nodes gr_gap = match acc_path with
        |[] -> (*beginning depth path search from id1 to id2*)
           ( match (out_arcs gr_gap id1) with
                |[]                -> (*id1 is isolated*)
                    acc_path
                |out_arcs_list -> (*iterating on the first node that is not 
                                in the marked_nodes list and marking the next node*)
                    ( match (first_non_marked out_arcs_list marked_nodes ) with
                        |None -> (*All next nodes have been marked (i.e. all path have been explored)*)
                            acc_path (*Algorithm ends here*)
                        |Some (id_next,value) -> (*Iterating on the next node*)
                            find_path id_next id2 [(id_next,value)] (id_next::marked_nodes) gr_gap))
                            
        |(id_next,value)::tl -> (*is path complete?*) 
            if (id_next == id2) then (*path is complete*) 
                acc_path
            else (*destination hasn't been reached yet*)
                ( match (out_arcs gr_gap id1) with
                |[]                -> (*id1 is isolated*)
                    acc_path
                |out_arcs_list -> (*iterating on the first node that is not 
                                in the marked_nodes list and marking the next node*)
                    ( match (first_non_marked out_arcs_list marked_nodes ) with
                        |None -> (*All next nodes have been marked (i.e. all path have been explored)*)
                            acc_path (*Algorithm ends here*)
                        |Some (id_next,value) -> (*Iterating on the next node*)
                            find_path id_next id2 [(id_next,value)] (id_next::marked_nodes) gr_gap))
