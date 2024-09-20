(* Problem 1 Solution *)
(* Author: Aiden *)
let cardinality lst =
  match lst with
  | [] -> 1
  | [_] -> 2
  | _ :: _ -> 0;;

(* Problem 2 Solution *)
(* Author: Aiden *)
let rotate lst =
  match lst with
  | [] -> []  (* No rotation for empty list *)
  | h :: t -> t @ [h]  (* Move head to the end *);;

(* Problem 3 Solution *)
(* Author: Aiden *)  
let remove_last lst =
  match List.rev lst with
  | [] -> []  (* Empty list *)
  | _ :: t -> List.rev t  (* Reverse again to drop the last element *);;

(* Problem 4 Solution *)
(* Author: Aiden *)
let rec remove_occurrences y lst =
  match lst with
  | [] -> []  (* Base case: empty list *)
  | h :: t -> if h = y then remove_occurrences y t  (* Skip element y *)
              else h :: remove_occurrences y t  (* Include the element *);;

(* Problem 5 Solution *)
(* Author: Aiden *)
let count_zeros_ones lst =
  let rec aux count_0 count_1 lst =
    match lst with
    | [] -> (count_0, count_1)  (* Return the counts *)
    | h :: t -> 
        if h = 0 then aux (count_0 + 1) count_1 t
        else if h = 1 then aux count_0 (count_1 + 1) t
        else aux count_0 count_1 t
  in
  aux 0 0 lst  (* Start with counts of 0 for both *);;