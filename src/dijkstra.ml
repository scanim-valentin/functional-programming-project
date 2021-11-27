open Graph
open Tools

let labelize gr_int = 
    (*Tags id wih an empty label*)
    let lbl_from_id id =  in 
    
    (*Tags id wih an empty label and appends lbl to arr*)
    let add_lbl arr id = Array.append arr (Array.make 1 (Inf, id, False) )  in
    
    (*Init lbl_array to Empty Array*)
    let lbl_array = Array.make 0 (Inf,0,False) in
    
    (*Folding on all nodes to make lbl_array*)
    n_fold gr_int add_lbl lbl_array
    
let dijkstra gr_int = 
    
    let lbl_array = labelize gr_int in
    
    
    
    
    
