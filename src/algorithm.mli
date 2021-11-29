open Graph
open Tools

(*type cost = Inf | Finite of int*)

val ford_fulkerson: (*int) graph -> id -> id -> (int*int) graph

val init_ff: int graph -> (int*int) graph

val gap_from_flow : (int*int) graph -> int graph

val add_double_arc : int graph -> id -> id -> (int*int) -> int graph

