open Graph
open Tools

(*type cost = Inf | Finite of int*)

val first_non_marked: (id * int) list -> id list -> (id * int) option

val flow_variation: (id * int) list -> int

val update_graph: id -> (id * 'a) list -> int graph -> int -> int graph

val find_path: id -> id -> (int) graph -> (id * int) list

val ford_fulkerson: (int) graph -> id -> id -> (int) graph