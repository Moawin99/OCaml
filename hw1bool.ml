(*Question 11*)
type bool_expr = 
    | Lit of string
    | Not of bool_expr
    | And of bool_expr * bool_expr
    | Or of bool_expr * bool_expr;;

let truth_table a b expr = 
    let rec solve c c_val d d_val = function
    | Lit x -> if x = c then c_val else if x = d then d_val else failwith "Invalid expression"
    | Not n -> solve c c_val d d_val n
    | And(a1, a2) -> solve c c_val d d_val a1 && solve c c_val d d_val a2
    | Or(o1, o2) -> solve c c_val d d_val o1 || solve c c_val d d_val o2
    in 
    [(true, true, solve a true b true expr);
     (true, false, solve a true b false expr);
     (false, true, solve a false b true expr);
     (false, false, solve a false b false expr)];;