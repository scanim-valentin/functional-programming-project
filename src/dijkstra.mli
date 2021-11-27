open Graph
open Tools

type cost = Inf | Finite of int

type label = (int * id * bool)

(*Generates a list of label from a graph of int, intialised to (Inf, id, False) *)
val labelize : int graph -> label Array

(*Will find a shortest path (i.e. a list of node) in a graph. Returns an empty list if no path exists between id1 and id2*)
val dijkstra : int graph -> id -> id -> id list

