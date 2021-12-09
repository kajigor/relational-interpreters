(* Naural numbers in Peano encoding *)
type nat = O | S of nat

(* Make these type declarations compile:

type graph     = (nat * nat) list
type numbering = nat list 
*)

(*
    Graphs are represented as lists of pairs of nodes; nodes are represented
    as natural numbers in the range [0..numberOfNodes-1].

    Numberings are represented as ordered lists of natural numbers where i-th 
    elements holds the number of i-th node

*)
                  
(* nat -> nat -> bool

   Tests the ordering relation on natural numers:
   
   less x y = true <=> x < y
*)                
let rec less x y =
  match y with
  | S y' -> (match x with
             | O    -> true
             | S x' -> less x' y'
            )
  | O -> false

let max x y =
  if less x y then y else x

let numberOfnodes g =
  let rec inner acc = function 
  | []           -> acc
  | (x, y) :: tl -> inner (max acc (max x y)) tl
  in
  inner O g
  
(* numbering -> nat -> nat

   Looks up an i-th element in a numbering
*)
let rec lookup (h :: tl) = function
| O   -> h
| S k -> lookup tl k

(* graph -> numbering -> bool

   Tests if given numbering for a given graph is a topologial sorting 
*)       
let eval graph numbering =
  let n = S (numberOfnodes graph) in
  let rec eval graph numbering =
    match graph with
    | []               -> true
    | (b, e) :: graph' -> let nb = lookup numbering b in
                          let ne = lookup numbering e in
                          less nb ne &&
                          less ne n &&
                          eval graph' numbering  
  in
  eval graph numbering
                      

                              
                            
                 
