(*Question 12*)
type expr =
    | Const of int
    | Var of string
    | Plus of {arg1:expr; arg2:expr}
    | Mult of {arg1:expr; arg2:expr}
    | Minus of {arg1:expr; arg2:expr}
    | Div of {arg1:expr; arg2:expr}

let expression = Plus {arg1 = (Mult {arg1 = Const 2; arg2 = Var "x"});
                       arg2 = (Mult {arg1 = Const 3; arg2 = (Minus {arg1 = Var "y"; arg2 = Const 1})})};;


(*Question 13*)
let rec evaluate expr = match expr with
| Const x -> x
| Plus{arg1 = a1; arg2 = a2} -> evaluate a1 + evaluate a2 
| Mult{arg1 = a1; arg2 = a2} -> evaluate a1 * evaluate a2
| Minus{arg1 = a1; arg2 = a2} -> evaluate a1 - evaluate a2
| Div{arg1 = a1; arg2 = a2} -> evaluate a1 / evaluate a2;;