(* Problem 1. [12 points] For each of the following OCaml function types, 
   define a function (with no explicit typing annotations, that is, no uses 
   of the : operator) for which OCaml would infer that type. The functions 
   need not be practical or do anything useful; they need only have the 
   requested type. For this problem, do not make use of anything from any 
   library modules other than Pervasives. Provide your answers in the boxes 
   provided below. (The first problem is done for you as an example.) *)

  (* (1) bool -> unit *)
  let f b = if b then () else () ;;

  (* (2) int -> int option *)
  let f a = Some (a + 1) ;;

  (* (3) 'a * bool -> 'a *)
  let f (a, b) =
    match (a, b) with
    | (a, true) -> a
    | (a, false) -> raise (Invalid_argument "invalid") ;;

  (* (4) 'a -> 'b list option *)
  let f a =
    Some [failwith "true"] ;;

  (* (5) ('a -> 'b) -> ('b -> 'c) -> ('a -> 'c) *)

  let f g h a =
    h (g a) ;;
  (* I don't know if this is right cuz I got 
  val f : ('a -> 'b) -> ('b -> 'c) -> 'a -> 'c = <fun>
  *)

(* ========================================================================= *)

(* Problem 2. [12 points] For each of the following top-level let definitions, 
   give a typing for the variable being defined that provides its most general 
   type (as would be inferred by OCaml) or explain briefly why the expression 
   is not well typed. (The first problem is done for you as an example.) *)
  
  (* (1) *)
  let f x = 
    x +. 42. ;;

  (* f : float -> float *)

  (* (2) *)
  let x = let y x = x in y ;;

  (* 'a -> 'a *)
  (* ask *)

  (* (3) *)
  let x = 3, 3 * 3, 3 ;;

  (* x : int * int int = (3, 9, 3) *)


  (* (4) *)
  let x = List.map ((+) 3.) [4., 5., 6.] ;;

  (* not well typed cuz uses int opertions on floats *)

  (* (5) *)
  let x = let open List in
          fun y -> filter ((=) y) ;;
  
  (* x : 'a -> 'a list -> 'a list *)

(* ========================================================================= *)

(* The Pervasives.string_of_bool function returns a string representation of a
   boolean. Here it is in operation:

  # string_of_bool (3 = 3) ;;
  - : string = "true"
  # string_of_bool (0 = 1) ;;
  - : string = "false" 
*)

(* Problem 3. [2 points] What is the type of string_of_bool? *)

(* string_of_bool : bool -> string *)


(* ========================================================================= *)

(* Problem 4. [6 points] Provide your own definition of the function 
   string_of_bool (without using Pervasives.string_of_bool of course). *)

let string_of_bool_own (=) : string =
  if (=) then "true"
  else "false" ;;

(* ========================================================================= *)

(* Recall that the Pervasives.compare function compares two values, returning 
   an int based on their relative magnitude: compare x y returns 0 if x is 
   equal to y, -1 if x is less than y, and +1 if x is greater than y.
   A function compare_lengths : ’a list -> ’b list -> int that compares the
   lengths of two lists can be implemented using compare by taking advantage 
   of the length function1 from the List module: *)

  let compare_lengths xs ys =
    compare (List.length xs) (List.length ys) ;;
  
  (* For instance, 
    # compare_lengths [1] [2; 3; 4] ;;
    - : int = -1
    # compare_lengths [1; 2; 3] [4] ;;
    - : int = 1
    # compare_lengths [1; 2] [3; 4] ;;
    - : int = 0
  *)
  
  (* However, this implementation of compare_lengths does a little extra work 
     than it needs to. Its complexity is O(n) where n is the length of the 
     longer of the two lists. *)

(* ========================================================================= *)

(* Problem 5. [6 points] Why does compare_lengths have this big-O complexity? 
   In particular, why does the length of the shorter list not play a part in 
   the complexity? We’re looking for a brief informal argument here, not a full 
   derivation of its complexity. *)




(* ========================================================================= *)

(* Problem 6. [8 points] Provide an alternative implementation of compare_lengths 
   whose com- plexity is O(n) where n is the length of the shorter of the two lists, 
   not the longer. *)

let rec compare_lengths_alt (lst1: 'a list) (lst2: 'b list) : int =
  match lst1, lst2 with
  | [], [] -> 0
  | _, [] -> 1
  | [], _ -> -1
  | _hd1 :: tl1, _hd2 :: tl2 -> compare_lengths_alt tl1 tl2 ;;



(* ========================================================================= *)

(* Problem 7. [8 points] Provide a derivation demonstrating that the expression 
    (fun x -> x + 2) 3

   evaluates to 5 according to the substitution semantics rules provided in 
   Figure 1. For your refer- ence, we’ve also provided the rules for substitution 
   in Figure 2.
*)




(* ========================================================================= *)

(* In this section, you’ll work with binary search trees, and learn about a 
   method for specifying particular nodes in trees due to computer pioneer Saul 
   Gorn (Figure 3(a)).

   Binary search trees (for the purpose of this section) are binary trees in 
   which (non-empty) nodes store a value, and further satisfy an invariant that 
   for a node with value x all of the values stored in its left subtree compare 
   (via the < operator) as less than x and all in the right subtree compare 
   greater than or equal to x. This allows efficient insertion into a tree and 
   searching in the tree for a given item.
  
   Here’s an algebraic data type for polymorphic binary search trees, where the 
   values stored at the nodes are of type ’a. *)

  type 'a bintree = 
  | Empty
  | Node of 'a bintree * 'a * 'a bintree ;;

(* We’ve provided in the box below an attempt at a function insert : 
   ’a -> ’a bintree -> ’a bintree to insert a new item into a binary search tree. 
   Unfortunately, there are several mistakes in the code that lead to errors, 
   warnings, or incorrect functioning. *)

  let insert (item : 'a) (tree : bintree) : bintree =
    match bintree with
    | Empty -> Node (Empty, item, Empty)
    | Node (left, old, right) -> 
        if old < item then
          Node (insert item left, right)
        else
          Node (left, insert item right) ;;

(* ========================================================================= *)

(* Problem 8. [9 points] Identify as many of these bugs as there are (but no 
   more), giving line num- bers for each and explaining what each problem is as 
   succinctly but specifically as you can and describing how it might be fixed. *)

(* 
  1: rec keyword missing
  1: bintree should be 'a bintree (twice)
  2: bintree should be tree
  5: if item < old
  6, 8: missing middle argument (old) for Node constructor
*)






(* ========================================================================= *)

(* Using the insert function (as corrected), we can, for instance, build the 
   tree depicted in  Figure 3(b) as follows:
  # let tr = Empty
  # |> insert 10
  # |> insert 5
  # |> insert 15
  # |> insert 7
  # |> insert 9 ;; 
  
    val tr : int bintree =
      Node (Node (Empty, 5, Node (Empty, 7, Node (Empty, 9,
      Empty))), 10,
        Node (Empty, 15, Empty))
*)

(* The Gorn address of a node in a tree (named after the early computer 
   pioneer Saul Gorn of University of Pennsylvania (Figure 3(a)), who invented 
   the technique) is a description of the path to take from the root of the 
   tree to the node in question. For a binary tree, the elements of the path 
   specify whether to go left or right at each node starting from the root of 
   the tree. We’ll define an enumerated type for the purpose of recording the 
   left/right moves. *)
  
  type direction = Left | Right ;;

(* Thus, for the tree tr defined above (depicted in Figure 3(b)), the Gorn 
   address of the root is [] and the Gorn address of the node containing the 
   item 9 is [Left, Right, Right]. *)

(* ========================================================================= *)

(* Problem 9. [10 points] Define a function gorn : 
   ’a -> ’a bintree -> direction list that given an item and a binary search 
   tree returns the Gorn address of the item in the tree. It should raise a 
   Failure exception if the item doesn’t occur in the tree. For instance, *)

(* 
  # gorn 9 tr ;;
  - : direction list = [Left; Right; Right]
  # gorn 10 tr ;;
  - : direction list = []
  # gorn 100 tr ;;
  Exception: Failure "gorn: item not found".
*)

let rec gorn (item : 'a) (tree : 'a bintree) : direction list =
  match tree with
  | Empty -> raise (Invalid_argument "gorn : item not found")
  | Node (left, old, right) ->
      if old = item then []
      else if item < old then 
        Left :: gorn item left
      else 
        Right :: gorn item right ;;

(* ========================================================================= *)

(* Problem 10. [6 points] Define a function find : ’a -> ’a bintree -> bool that 
   given an item and a binary search tree returns true if the item is in the tree 
   and false otherwise. You may want to use the gorn function in defining find. 
   Examples of the find function in use include: 
   
   # find 9 tr ;;
   - : bool = true
   # find 100 tr ;;
   - : bool = false   

*)

let find item tree =
  try
    let _ = gorn item tree
    in true
  with
  | Failure _ -> false ;;

(* remember that you can use "try ... with ..."! *)






(* ========================================================================= *)







(* ========================================================================= *)








(* ========================================================================= *)










(* ========================================================================= *)








(* ========================================================================= *)











(* ========================================================================= *)
