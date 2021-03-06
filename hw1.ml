(*Question 1*)
let rec pow x n = if n = 0 then x else pow (x*x) (n-1);;

let rec float_pow x n = if n = 0 then x else float_pow (x*.x) (n-1);;

(*Question 2*)
let rec compress = function
    | a :: (b :: _ as t) -> if a = b then compress t else a :: compress t
    | lst -> lst;;


(*Question 3*)
let rec remove_if lst f = match lst with
| h::t -> if (f h) then remove_if t f else h::remove_if t f
| [] -> [];;

(*Question 4*)
let slice lst i j = 
    let rec leave n = function  
    | [] -> []
    | h::t -> if n = 0 then [] else if n < 0 then [] else h:: leave (n-1) t
    in
    let rec remove n = function
    | [] -> []
    | h::t as l -> if n = 0 then l else remove (n-1) t
    in
    leave (j - i + 1) (remove i lst);; 
