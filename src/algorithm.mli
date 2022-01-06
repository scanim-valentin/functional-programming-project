open Graph
open Tools

(*type cost = Inf | Finite of int*)

val ford_fulkerson: (int) graph -> id -> id -> (int) graph

val find_path : id -> id -> (int) graph -> (id * int) list

val flow_variation : (id*int) list -> int

val update_graph : id -> (id * 'a) list -> int graph -> int -> int graph
