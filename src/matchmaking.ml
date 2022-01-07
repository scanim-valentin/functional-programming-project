open Gfile
open Graph
open List
open Tools
open Algorithm
open Printf
let () =

(*Reads a file with a list of names (separated by \n) and returns a list of names (string)*)
let from_file_names path =

  let infile = open_in path in

  (* Read all lines until end of file. *)
  let rec loop name_list =
    try
      let line = input_line infile in

      (* Remove leading and trailing spaces. *)
      let line = String.trim line in

      (* Ignore empty lines *)
      if line = "" then name_list
      (* Adding the name to the list *)
      else loop (line::name_list)

    with End_of_file -> name_list (* Done *)
  in

  let final_list = loop [] in

  close_in infile ;
  final_list in

(*Initiate a bipartite graph based on a given list of names (each of them are "duplicated" to finally form pairs of names)*)
let init_matchmaking name_list = 

  (*returns an association list between an even number and a name *)
  (*assoc_list: (int,string) list*)
  let rec init_assoc list assoc_list i = 
    match list with
      |[] -> assoc_list
      |name::tl -> init_assoc tl ((2*i,name)::assoc_list) (i+1) in
  
  (*Generates the recipient nodes*)
  let fold_init_graph_1 graph (i,name) = 
    let () = printf "init_graph_1 - Creating node %d\n%!" i in
    new_node graph i in
  
  (*Links the i-th node with every recipient nodes except the one associated to the same name*)
  let fold_link_to_others i graph (index,name) = 
    if(i = index)then
    graph
    else
      let () = printf "init_graph_2 - Linking to node %d \n%!" index in 
      (*i-1 because i correspond to a recipient node and therefore is even*)
      new_arc graph (i-1) index 1 in
  
  (*Links buyer and recipient nodes between them*)
  let fold_init_graph_2 assoc_list graph (i,name) = 
    let () = printf "init_graph_2 - Creating node %d \n%!" (i-1)in
    let new_graph = new_node graph (i-1) in
    fold_left (fold_link_to_others i) new_graph assoc_list in
  
  (*Associating each name to an index*)
  let assoc = init_assoc name_list [] 1 in
  let final_graph = new_node empty_graph 0 in
  (*Creating recipient nodes*)
  let final_graph = fold_left fold_init_graph_1 final_graph assoc in
  (*Creating buyer nodes and linking them to each recipient node*)
  let final_graph = fold_left (fold_init_graph_2 assoc) final_graph assoc in
  (*Creating source and sink nodes*)
  
  let last_id = 2*(length assoc)+1 in
  let () = printf " Creating sink %d \n%!" last_id in
  let final_graph = new_node final_graph last_id in
  let final_graph = fold_left (fun graph (index,_) -> new_arc graph 0 (index-1) 1) final_graph assoc in
  (fold_left (fun graph (index,_) -> new_arc graph index last_id 1) final_graph assoc,assoc) in


  (*Converts a graph to a dot file*)
let export_matchmaking path graph assoc_list =

  (* Open a write-file. *)
  let ff = open_out path in

  (* Write in this file. *)
  fprintf ff "digraph my_graph {
  rankdir=LR;
  size=\"100,100\"
  node [shape = circle];\n" ;

  (* Write all arcs *)
  let fold_print nul1 id1 id2 lbl = 
    if(id1 = 0)then
      let name2 = assoc (id2+1) assoc_list in
      fprintf ff "    s -> %s [label=\"%s\"];\n" name2 lbl 
    else if(id2 = (2*(length assoc_list)+1))then
      let name1 = assoc id1 assoc_list in
      fprintf ff "    %s2 -> t [label=\"%s\"];\n" name1 lbl 
    else if(id2 = 0)then
      let name1 = assoc (id1+1) assoc_list in
      fprintf ff "    %s -> s [label=\"%s\"];\n" name1 lbl 
    else if(id1 = (2*(length assoc_list)+1))then
      let name1 = assoc id2 assoc_list in
      fprintf ff "    t -> %s2 [label=\"%s\"];\n" name1 lbl 
    else if((id2 mod 2) = 0)then
      let name1 = assoc (id1+1) assoc_list in
      let name2 = assoc id2 assoc_list in
      fprintf ff "    %s -> %s2 [label=\"%s\"];\n" name1 name2 lbl 
    else
      let name1 = assoc id1 assoc_list in
      let name2 = assoc (id2+1) assoc_list in
      let () = printf "  %s offre un cadeau à %s \n%!" name1 name2 in
      fprintf ff "    %s2 -> %s [label=\"%s\"];\n" name1 name2 lbl in

  let _ = e_fold graph fold_print () in

  fprintf ff "}\n" ;

  close_out ff ;
  () in

  (* Check the number of command-line arguments *)
  if Array.length Sys.argv <> 3 then
    begin
      Printf.printf
        "\n âœ»  Usage: %s infile outfile\n\n%s%!" Sys.argv.(0)
        ("      infile  : input file containing a list of names\n" ^
         "      outfile : output file in which the result should be written.\n\n") ;
      exit 0
    end ;


  (* Arguments are : infile(1) source-id(2) sink-id(3) outfile(4) *)

  let infile = Sys.argv.(1)
  and outfile = Sys.argv.(2)

  in


  (* Open file *)
  let () = printf "Input file : %s Output file : %s\n%!" infile outfile in

  let name_list = from_file_names infile in
  let () = List.iter (printf "%s %!") name_list in
  let () = printf "\n%!" in

  match init_matchmaking name_list with
    |(g,a) -> let graph = g in let assoc_list = a in
  
  let () = export_matchmaking (outfile^"_init.dot") (gmap graph string_of_int) assoc_list in 
  let () = write_file (outfile^"_init") (gmap graph string_of_int) in
  let graph = ford_fulkerson graph 0 (2*(length assoc_list)+1) in

  let graph = gmap graph string_of_int in
  (* Rewrite the graph that has been read. *)
  let () = write_file outfile graph in
  (*Printing the result of the matchmaking*)
  let () = printf "\n\n SECRET SANTA : \n%!" in
  let () = export_matchmaking (outfile^".dot") graph assoc_list in 

  ()