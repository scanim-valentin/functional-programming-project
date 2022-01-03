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
let flow_variation pth = 
  let folding acc (_,value) = 
    (match acc with 
      |None -> Some value (*Fist call case*)
      |Some x -> if(value < x)then
          Some value
        else
          Some x) in
        
  match (List.fold_left folding None pth) with
    |None -> 0
    |Some min -> min

let rec update_graph path gr_int mn = match path with
  |[] -> gr_int
  |[(id1,val1)] -> gr_int
  |(id1,val1)::((id2,val2)::tl) -> (*Updating the out and in arc values along the given path*)
    let new_gr_int = add_arc gr_int id1 id2 (-mn) in 
    add_arc new_gr_int id2 id1 mn

(*Finds a path (list of (arc id)) between source and sink *)
let find_path source sink graph =

  (*acc_path : the path to complete ; marked_nodes : list of marked nodes to avoid*)
  let rec step acc_path marked_nodes current src sk graph =
    
    (*Checking if destination has been reached*)
    if(current = sk)then 
      acc_path
  else
    (*Finding the next node that hasn't been marked*)
    match(first_non_marked (out_arcs graph current) marked_nodes)with
      |None -> (*current is isolated (i.e. we need to go back)*)
        (match(acc_path)with
          |[] -> (*path is empty which means there is no available path (current = source)*)
            []
          |last::prev_tail -> (*path is not empty which means we can go back (i.e. pop the path's head)*)
            (match(prev_tail)with
              |[] -> (*back at the source node*)
                step [] (current::marked_nodes) src sk src graph
              |(previous,_)::tl -> (*marking this node and stepping back into the previous node*)
                step tl (current::marked_nodes) previous sk src graph
            ) 
        )
      |Some (next,w) -> (*stepping into the next non marked node*)
        step ((next,w)::acc_path) (current::marked_nodes) next src sk graph in

  step [] [] source source sink graph

(*Implementing the Ford-Fulkerson algorithm*)
let ford_fulkerson grph id1 id2 =
  
  let rec iter src sk graph i =
    let path = find_path src sk graph in
    let mapped = List.map (fun (idN, value) -> sprintf "(id %d, value%d)" idN value) path in
    let path_string = String.concat "<-" mapped in
    let () = printf "%s\n%!" path_string in
    match path with 
    |[] -> 
      let () = printf "saucisse\n%!" in 
      graph
    |_::_ -> (*A path exists: updating the graph based on the minimal flow*)
      let flow_min = flow_variation path in
      let () = printf "Flow min : %d\n%!" (flow_min) in 
      let updated_graph = update_graph path graph (flow_min) in
      if(flow_min = 0)then
        graph
      else
      iter src sk updated_graph 0 in
  iter id1 id2 grph 0

(* OLD
let ford_fulkerson gr_int id1 id2 =
  let gr_flow = init_ff gr_int in
  let gr_gap = gap_from_flow gr_flow in

  let rec iter src sk gr_gp i =
    let path = find_path src sk gr_gp in
    let mapped = List.map (fun (idN, value) -> sprintf "(%d,%d)" idN value) path in
    let path_string = String.concat "<-" mapped in
    let () = printf "%s\n%!" path_string in
    match path with 
    |[] -> 
      let () = printf "saucisse\n%!" in 
      gr_gap
    |_::_ -> (*A path exists: updating the graph based on the minimal flow*)
      let flow_min = flow_variation path 0 in
      let () = printf "Flow min : %d\n%!" (flow_min) in 
      let updated_graph = update_graph path gr_gp (flow_min) in
      iter src sk updated_graph 0 in
  
  iter id1 id2 gr_gap 0

*)