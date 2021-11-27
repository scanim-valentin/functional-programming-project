open Graph
open Tools

type cost = Inf | Finite of int
type label = (int * id * bool)

(*Generates a list of label from a graph of int, intialised to (Inf, id, False) *)
val labelize : int graph -> label Array

val dijkstra : int graph -> id -> id -> id list

