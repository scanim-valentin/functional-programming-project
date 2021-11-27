open Graph
open Tools
open Dijkstra
(*val ford_fulkerson: 'a graph -> id -> id -> int*)

val init_ff: int graph -> (int*int) graph

val gr_gap : (int*int) graph -> int graph

val add_double_arc : int graph -> id -> id -> (int*int) -> int graph

