(* Problem 11a Solution *)
(* Author: Sawyer*)
type float_tree =
  | Empty
  | Node of float * float_tree * float_tree
(* Problem 11b Solution *)
(* Author: Sawyer*)
let create_tree () =
  Node (5.0,
    Node (3.14, 
      Node (1.41, Empty, Empty),  
      Node (2.71, Empty, Empty)), 
    Node (6.28, 
      Node (1.61, 
        Empty, 
        Node (1.73, Empty, Empty)), 
      Empty)) 
(* Problem 12 Solution *)
(* Author: Sawyer*)
let rec tree_depth tree =
  match tree with
  | Empty -> 0  
  | Node (_, left, right) ->
      let left_depth = tree_depth left in
      let right_depth = tree_depth right in
      1 + max left_depth right_depth
(* Problem 13 Solution *)
(* Author: Sawyer*)
let rec count_leaves tree =
  match tree with
  | Empty -> 0  
  | Node (_, Empty, Empty) -> 1  
  | Node (_, left, right) -> 
      count_leaves left + count_leaves right  