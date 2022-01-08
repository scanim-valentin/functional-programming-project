open Gfile
open Tools
open Algorithm
open Printf


let () =
  (* Check the number of command-line arguments *)
  if Array.length Sys.argv <> 5 then
    begin
      Printf.printf
        "\n âœ»  Usage: %s infile source sink outfile\n\n%s%!" Sys.argv.(0)
        ("    ðŸŸ„  infile  : input file containing a graph\n" ^
         "    ðŸŸ„  source  : identifier of the source vertex (used by the ford-fulkerson algorithm)\n" ^
         "    ðŸŸ„  sink    : identifier of the sink vertex (ditto)\n" ^
         "    ðŸŸ„  outfile : output file in which the result should be written.\n\n") ;
      exit 0
    end ;
  (* Arguments are : infile(1) source-id(2) sink-id(3) outfile(4) *)

  let infile = Sys.argv.(1)
  and outfile = Sys.argv.(4)
  and source = int_of_string Sys.argv.(2)
  and sink = int_of_string Sys.argv.(3)
  in

  (*Open file *)
  let graph = from_file infile in

  let graph = gmap graph int_of_string in

  (*Apply ford_fulkerson on graph from source to sink *)
  let graph = ford_fulkerson graph source sink in

  let graph = gmap graph string_of_int in

  (*Rewrite the graph that has been read *)
  let () = write_file outfile graph in

  let () = export (outfile^".dot") graph in

  ()
