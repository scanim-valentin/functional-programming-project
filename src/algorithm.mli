open Graph
open Tools

(*val ford_fulkerson: 'a graph -> id -> id -> int*)

val init_ff: int graph -> (int*int) graph

val gr_gap : (int*int) graph -> int graph

val add_double_arc : (int*int) graph -> id -> id -> int -> int graph