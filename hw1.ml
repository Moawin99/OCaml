(*Question 1*)
let rec pow x n = match n with 
| 0 -> 1
| _ ->  x * pow x (n-1);;

let rec float_pow x n = match n with
| 0 -> 1.0
| _ -> x *. float_pow x (n-1);;

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

(*Question 5*)
let equivs f lst =
    let rec checkPartition f head arr = match arr with 
    | [] -> [[head]]
    | h1::t -> match h1 with 
        | [] -> []
        | h::_ -> if (f h head) then (h1@[head])::t else h1::(checkPartition f head t)
    in
    let rec createPartition f tail array = match tail with
    | [] -> array
    | h::t -> createPartition f t (checkPartition f h array)
    in 
    match lst with
    | [] -> []
    | h::t -> (createPartition f t [[h]]);;


(*Question 6*)
let goldbachpair num =
   let prime n =
    let rec checkZero x d = match d with
        | 1 -> true    
        | _ -> (x mod d <> 0) && checkZero x (d-1)
    in match n with
    | 0 | 1 -> false
    | _ -> checkZero n (n-1)
    in
    let rec aux d =
    if prime d && prime (num - d) then (d, num - d) else aux (d + 1) 
    in
    aux 2;;


(*Question 7*)
let rec equiv_on f g lst = match lst with
| [] -> true
| h::t -> if ((f h) = (g h)) then equiv_on f g t else false;;


(*Question 8*)
let rec pairwisefilter f lst = match lst with
| [] -> []
| [fst] -> fst::[]
| h::(b::e) -> (f h b)::pairwisefilter f e;;


(*Question 9*)
let rec polynomial lst e = match lst with
| [] -> 0
| (x,y)::t -> ((pow e y) * x) + polynomial t e;;


(*Question 10*)
