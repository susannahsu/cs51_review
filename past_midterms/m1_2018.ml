
(* Problem 1. [12 points] For each of the following OCaml function types define 
   a function (with no explicit typing annotations, that is, no uses of the 
   : operator) for which OCaml would infer that type. The expressions need not 
   be practical or do anything useful; they need only have the requested type. 
   Do not make use of anything from any library modules other than Pervasives. 
   Provide your answers in the boxes provided below. (The first problem is done 
   for you as an example.) *)

  (* (1) bool -> unit *)
  let f b = if b then () else () ;;

  (* (2) 'a option list -> bool *)
  let f lst =
  match lst with
  | [] -> false
  | [None] -> false
  | _ -> true ;;
  
  (* (3) 'a list -> 'a option *)
  let f lst = 
  match lst with
  | [] -> None
  | [a] -> Some a 
  | _ -> None ;;

  (* (4) (bool option -> bool) -> bool *)
  let f g = g (Some true) ;;

  (* (5) 'a -> 'a -> 'b *)
  let f x y =
    if x = y then failwith "true" else failwith "false" ;;
  (* Raise an exception instead of returning *)

(* ========================================================================= *)

(* Problem 2. [12 points] For each of the following function definitions, give 
   a typing for the function that provides its most general type (as would be 
   inferred by OCaml) or explain briefly why no type exists for the function. 
   (The first problem is done for you as an example.) *)

  (* (1) *)
  let f1 x =
    x +. 42. ;;

  (* f1 : float -> float *)

  (* (2) *)
  let f2 x y =
    x y y ;;

  (* f2: ('a -> 'a -> 'b) -> 'a -> 'b *)

  (* (3) *)
  let f3 x y =
    x (y y) ;;

  (* This definition does not type. y is here applied as a function, so its 
     type must be of the form ’a -> ’b. Yet the function y can take y as an 
     argument. This implies that ’a, the type of the input to y, must be 
     identical to’a -> ’b, the type of y itself. There is no finite type 
     satisfying that constraint. A type cannot be a subpart of itself. *)
  
  (* (4) *)
  let rec f4 x =
    match x with
    | None
    | Some 0 -> None
    | Some y -> f4 (Some (y - 1)) ;;
  (* The code matches x with option types formed with Some or None, so we know
  that x must be of type ’a option for some ’a. We also see that when 
  deconstructing x into Some y, we perform subtraction on y in the recursive
  function call: f (Some (y - 1)). We can thus conclude y is of type int, and 
  can further specify x to be of type int option. Finally,note that the case
  None | Some 0 -> None is the sole terminal case in this recursive function. 
  Because this case returns None, we know that if f terminates, f returns None. 
  Our function f therefore outputs a value of type ’a option. We cannot infer a 
  more specific type for ’a because we always return None and thus have no 
  constraints on ’a. The final typing is thus as follows:
  f : int option -> ’a option. *)
  (* f4: int option -> 'a option *)

  (* (5) *)
  let f5 x y =
    if x then [x]
    else [not x; y] ;;
  (* x is a bool.
  f5: bool -> bool -> bool list *)

(* ========================================================================= *)

(* Problem 3. [4 points] Define a function even that returns true if its integer
   argument is even, and false otherwise. *)

  let even (n : int) : bool =
    n mod 2 = 0 ;;


(* ========================================================================= *)

(* Problem 4. [2 points] What is the type of even? *)

  (* even: int -> bool *)

(* ========================================================================= *)

(* Problem 5. [3 points] Define a function odd that returns true if its integer 
   argument is odd, and false otherwise. You’ll want to make use of the function 
   even you just defined in Problem 3. *)

  let odd (x : int) : bool =
    not (even x) ;;




(* ========================================================================= *)

(* The OCaml documentation for the List module describes a function called 
   partition: *)

  (* val partition : ('a -> bool) -> 'a list -> 'a list * 'a list *)
      (* partition p l returns a pair of lists (l1, l2), where l1 is the
      list of all the elements of l that satisfy the predicate p, and l2 is 
      the list of all the elements of l that do not satisfy p. The order of
      the elements in the input list is preserved. *)
  
(* 
For example, using the even function from Problem 3, the following should hold: 
    # partition even [1; 2; 3; 4; 5; 6; 7] ;;
    - : int list * int list = ([2; 4; 6], [1; 3; 5; 7])
*)

(* ========================================================================= *)

(* Problem 6. [6 points] Give your own definition of the partition function, 
   implemented directly without use of any library functions except for those in 
   the Pervasives module. *)

let rec partition (test : 'a -> bool) (lst : 'a list) : 'a list * 'a list =
  match lst with
  | [] -> [], []
  | hd :: tl ->
      let yes, no = partition test tl in
        if test hd then hd :: yes, no
        else yes, hd :: no ;;




(* ========================================================================= *)

(* The artist Alexander Calder (1898–1976) is well known for his distinctive 
   mobiles, sculp- tures with different shaped objects hung from a cascade of 
   connecting metal bars. An example is shown in Figure 1.

   His mobiles are made with varying shapes at the ends of the connectors – 
   circles, ovals, fins. The exquisite balance of the mobiles depends on the 
   weights of the various components. In the next sections of the exam, you 
   will model the structure of mobiles as binary trees to allow determining if 
   a Calder-like mobile design is balanced or not. *)

(* Let’s start with the objects at the ends of the connectors. For our purposes, 
   the important properties of an object will be its shape and its weight (in 
   arbitrary units; you can interpret them as pounds). *)

(* ========================================================================= *)

(* Problem 7. [2 points] Define a weight type consisting of a single floating 
   point weight. *)

  type weight = float ;;

(* ========================================================================= *)

(* Problem 8. [3 points] Define a shape type, a variant type that allows for 
   three different shapes: circles, ovals, and fins. (No information beyond the 
   type of shape need be provided for.) *)

  type shape =
  | Circles
  | Ovals
  | Fins ;;


(* ========================================================================= *)

(* Problem 9. [3 points] Define an obj type that will be used to store information 
   about the objects at the ends of the connectors, in particular, their weight 
   and their shape. *)

  type obj = {weight : weight; shape : shape} ;;


(* ========================================================================= *)

(* Problem 10. [2 points] To demonstrate the use of the types you’ve just defined, 
   define a value shape1 : obj that represents an oval of weight 8. *)

  let shape1 = {weight = 8. ; shape = Ovals} ;;


(* ========================================================================= *)

(* A mobile can be modeled as a kind of binary tree, where the leaves of the tree, 
   rep- resenting the objects, are elements of type obj, and the internal nodes, 
   representing the connectors, have a weight, and each internal node (connector) 
   connects two submobiles. Rather than directly writing code for a mobile type, 
   though, we’ll digress to build a more general binary tree module, and then model 
   mobiles using that. *)

(* An appropriate signature BINTREE for a simple binary tree module might be the 
   follow- ing: *)

   module type BINTREE
    sig
      type leaft (* the type for the leaves of the tree *)
      type nodet (* the type for the internal nodes of the tree *)
      type tree (* the type for the trees themselves *)

      val make_leaf : leaft -> tree
      val make_node : nodet -> tree -> tree -> tree
      val walk : (leaft -> 'a) -> (nodet -> 'a -> 'a -> 'a) -> tree -> 'a
    end ;;

(* This module signature specifies separate types for the leaves of trees and 
   the internal nodes of trees, along with a type for the trees themselves; 
   functions for constructing leaf and node trees; and a single function to 
   “walk” the tree. (We’ll come back to the walk function later.) *)

(* In addition to the signature for binary tree modules, we would need a way 
   of gener- ating implementations of modules satisfying the BINTREE signature, 
   which we’ll do with a functor MakeBintree. The MakeBintree functor takes an 
   argument module of type BINTREE_ARG that packages up the particular types for 
   the leaves and nodes, that is, the types to use for leaft and nodet. The 
   following module signature will work: *)

   module type BINTREE_ARG =
    sig
      type leaft
      type nodet
    end ;;

(* ========================================================================= *)

(* Problem 11. [6 points] Define the header of a functor named MakeBintree to 
   generate modules satisfying the BINTREE signature by filling in the template 
   below, keeping in mind the need for users of the functor-generated modules to
   access appropriate aspects of the generated trees. *)

  module MakeBinTree (Element : BINTREE_ARG)
    : (BINTREE with
        type leaft = Element.leaft and
        type nodet = Element.nodet) =

    struct

      (* ... the implementation would go here,
             but you don't have to worry about that ... *)

    end ;;



(* ========================================================================= *)

(* Using this functor, you can now generate a Mobile module, which has objs at 
   the leaves and weights at the interior nodes. *)

(* Problem 12. [5 points] Define a module Mobile using the functor MakeBintree. *)

  module Mobile =
    MakeBinTree (struct 
                  type leaft = obj
                  type nodet = weight
                end) ;;

(* an alternative is to explicitly define the argument values: *)
  module MobileArg =
    struct
      type leaft = obj
      type nodet = weight
  end ;;

(* it can also be: *)
  module MobileArg : (BINTREE_ARG with type leaft = obj
                                   and type nodet = weight) =
    struct
      type leaft = obj
      type nodet = weight
    end ;;

(* ========================================================================= *)

(* Problem 13. [2 points] You’ve just used the MakeBintree functor without ever 
   seeing its implementation. Why is this possible? *)

(* The only aspects pertinent to the use of a module are manifest in the signature.
   A user need not know how a module of type BINTREE, say, makes a leaf; a user
   only needs to know the signature of the make_leaf function in order to use it.
   A user in fact cannot access the implementation details because we've constrained
   the module to the BINTREE interface.
   
   Similarly, a user need not know how the functor MakeBintree works, as
   implementation details would not be accessible to the user anyway. So long as a
   user knows the functor's signature, they know if they pass in any module following
   the BINTREE_ARG signature, the functor will return a module following the BINTREE
   signature. *)


(* ========================================================================= *)

(* You can now build a representation of a mobile using the functions that the 
   Mobile module makes available. *)

(* Problem 14. [4 points] Define a value mobile1 of type Mobile.tree that 
   represents a mobile structured as the one in Figure 2. *)

  let mobile1 =
    let open Mobile in

    make_node
      1.0
      (make_leaft {weight = 9.0 ; shape = Ovals})
      (make_nodet 
        1.0
        (make_leaft {weight = 3.5; shape = Fins})
        (make_leaft {weight = 4.5; shape = Fins})) ;; 



(* ========================================================================= *)

(* The walk function, of type (leaft -> ’a) -> (nodet -> ’a -> ’a -> ’a) -> tree -> ’a, 
   is of special interest, since it is the sole method for performing computations 
   over these binary trees. The function is a kind of fold that works over trees 
   instead of lists. It takes two functions – one for leaves and one for nodes – and 
   applies these functions to a tree to generate a single value. The leaf function 
   takes a leaft and returns some value of type ’a. The node function takes a nodet 
   and the two ’a values recursively returned by walking its two subtrees and computes 
   the value for the node itself. *)

(* For example, we can use walk to define a function size that counts how many objects 
   there are in a mobile. The function uses the fact that leaves are of size 1 and the 
   size of a non-leaf is the sum of the sizes of its subtrees. *)

  let size mobile =
    Mobile.walk (fun _leaf -> 1)
                (fun _node left_size right_size -> left_size + right_size)
                mobile ;;

(* ========================================================================= *)

(* Problem 15. [3 points] What is the type of size? *)

(* The size function takes in a binary tree representing a mobile and returns
   the number of leaves in that tree. The type is thus Mobile.tree -> int *)

(* ========================================================================= *)

(* Problem 16. [2 points] Use the fact that the walk function is curried to give 
   a slightly more concise definition for size. *)

  let size =
    Mobile.walk (fun _leaf -> 1)
                (fun _node left_size right_size ->
                  left_size + right_size) ;;

  (* We pass in mobile as an argument to size, only to just pass it in again as
     the last argument to Mobile.walk; partial application allows us to simplify. *)
(* ========================================================================= *)

(* Problem 17. [5 points] Use the walk function to implement a function shape_count : 
   shape -> Mobile.tree -> int that takes a shape and a mobile (in that order), 
   and returns the number of objects in the mobile that have that particular shape. *)

  let shape_count (s: shape) : int =
    Mobile.walk (fun leaf -> if leaf.shape = s then 1 else 0)
                (fun _node left_count right_count ->
                  left_count + right_count) ;;

(* ========================================================================= *)

(* A mobile will said to be balanced if every connector has the property that the 
   total weight of all components (that is, objects and connectors) of its left 
   submobile is the same as the total weight of all components of its right 
   submobile. (In actuality, we’d have to worry about other things like the 
   relative lengths of the arms of the connectors, but we’ll ignore all that.) *)


(* ========================================================================= *)

(* Problem 18. [2 points] Is the mobile shown in Figure 2 balanced? Why or 
   why not? *)

(* No, this mobile is not balanced. The right subtree connects two submobiles of
   different weights (3.5 and 4.5). *)

(* ========================================================================= *)

(* Problem 19. [7 points] Implement a function balance : 
   Mobile.tree -> weight option that takes a mobile, and returns None if the 
   argument mobile is not balanced, and Some w if the mobile is balanced, 
   where w is the total weight of the mobile. *)

  let balance (mobile : Mobile.tree) =
    Mobile.walk (fun leaf -> Some leaf.weight)
                (fun node left right ->
                  match left, right with
                  | Some wt1, Some wt2 ->
                      if wt1 = wt2 then
                        Some (node +. wt1 +. wt2)
                      else None
                  | _, _ -> None
                mobile ;;




