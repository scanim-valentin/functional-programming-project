open Graph
open Gfile
open Tools
open Printf

(*Initializes the flow for every arc*)
let init_ff gr_int = gmap gr_int ( fun lbl->(0,lbl) ) 
let add_double_arc gr_int n1 n2 (lbl1,lbl2) = 
  let gr_r = add_arc gr_int n2 n1 lbl1 in
  add_arc gr_r n1 n2 lbl2 
(*Returns the gap graph*)

(* val e_fold: 'a graph -> ('b -> id -> id -> 'a -> 'b) -> 'b -> 'b *)
(*             (src)        (acc) (n1)  (n2)  (data arc)   (acc)  *)
let gap_from_flow gr_flow = e_fold gr_flow add_double_arc (clone_nodes gr_flow)

(*Returns the first out arc to a non marked AND strictly positive node or none if all have been marked*)
let rec first_non_marked out_arc_lst marked_nodes_lst =
        let cond_arc (idA,value) = if (value = 0) then
                false
            else 
                match (List.find_opt ( fun idB -> (idB = idA) ) marked_nodes_lst ) with
                    |None -> true 
                    |Some id -> false in
    
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
        |[(id1,val1)] -> gr_int
        |(id1,val1)::((id2,val2)::tl) -> (*Updating the out and in arc values along the given path*)
            let new_gr_int = add_arc gr_int id1 id2 (-mn) in 
            add_arc new_gr_int id2 id1 mn
            

                
(*Finds a path in a graph between id1 and id2 based on depth search*)
let rec find_path id1 id2 acc_path marked_nodes gr_gap = 
    let deb = (printf "find_path from node %d to node %d\n%!" id1 id2) in
    let iter_from idN =
            let deb = (printf "iter_from node %d\n%!" idN) in
            ( match (out_arcs gr_gap idN) with
                |[]                -> (*id1 is isolated*)
                    let deb = (printf "out_arcs gr_gap idN = []\n%!") in
                    acc_path
                    (*printf "\n%s%!" "Isolated"*)  
                |out_arcs_list -> (*iterating on the first node that is not 
                                in the marked_nodes list and marking the next node*)
                    let deb = (printf "out_arcs is not empty \n%!") in
                    
                    ( match (first_non_marked out_arcs_list marked_nodes ) with
                        |None -> (*All next nodes have been marked (i.e. all path have been explored)*)
                            let deb = (printf "all nodes have been marked\n%!") in
                            acc_path (*Algorithm ends here*)
                        |Some (id_next,value) -> (*Iterating on the next node*)
                            (*printf "Next node : %d\n%!" id_next;  *)
                            let deb = (printf "next non marked is node %d \n%!" id_next) in
                            find_path id_next id2 ((id_next,value)::acc_path) (id_next::marked_nodes) gr_gap)) in
    match acc_path with
        |[] -> (*beginning depth path search from id1 to id2*)
                        iter_from id1
        |(id_next,value)::tl -> (*is path complete?*) 
            if (id_next == id2) then (*path is complete*) 
                acc_path
            else (*destination hasn't been reached yet*)
                
                iter_from id_next


(*Implementing the Ford-Fulkerson algorithm*)
let ford_fulkerson gr_int id1 id2 =
    let gr_flow = init_ff gr_int in
    let gr_gap = gap_from_flow gr_flow in

    let rec iter src sk gr_gp i =
        let i = i + 1 in
        let path = find_path src sk [] [] gr_gp in

        let mapped = List.map (fun (idN, value) -> sprintf "(%d,%d)" idN value) path in
        let path_string = String.concat "<-" mapped in
        let deb = printf "%s\n%!" path_string in
        if(i < 10)then
            let outfile = "outfile"^(string_of_int i) in
            let f = write_file outfile gr_gp in
            let f = export (outfile^".dot") gr_gap in

        match path with
            |[] -> gr_gp
            |_::_ -> iter src sk ( update_graph path gr_gp (flow_variation path 0) ) 0 in

    iter id1 id2 gr_gap 0
