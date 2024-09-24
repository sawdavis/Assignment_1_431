(*Problem 6 Solution*)
(*Author: Jared*)
let rec makepairs x l = 
  List.map (fun y -> (x,y)) l

(*Problem 7 Solution*)
(*Author: Jared*)
let rec bi_coefficients n k = 
  match n, k with
  | n, 0 -> 1
  | n, k when n = k -> 1
  | n, k -> bi_coefficients (n-1) (k-1) + bi_coefficients (n-1) k;;

(*Problem 8 Solution*)
(*Author: Jared*)
let rec dup lst = 
  match lst with
  | [] -> []
  | h::t -> h::h::dup t;;

(*Problem 9 Solution*)
(*Author: Jared*) 
let rec undup lst =
  match lst with
  | [] -> []
  | h1::h2::t when h1=h2 -> h1::undup t
  | _ -> raise (Failure "bad input")

(*Problem 10 Solution*)
(*Author: Jared*)
let rec smallest lst = 
  match lst with
  | [] -> raise (Failure "Empty List")
  | [h] -> h
  | h::t -> min h (smallest t);;