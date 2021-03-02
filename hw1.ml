(*Question 1*)
let rec pow x n = if n = 0 then x else pow (x*x) (n-1);;

let rec float_pow x n = if n = 0 then x else float_pow (x*.x) (n-1);;

(*Question 2*)
let rec compress = function
| a :: (b :: _ as t) -> if a = b then compress t else a :: compress t
| lst -> lst;;