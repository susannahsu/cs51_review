(* Problem 1. [12 points] For each of the following OCaml function types 
   define a function (with no explicit typing annotations, that is, no uses 
   of the : operator) for which OCaml would infer that type. The expressions 
   need not be practical or do anything useful; they need only have the 
   requested type. Do not make use of anything from any library modules other 
   than Pervasives. Provide your answers in the boxes provided below. (The 
   first problem is done for you as an example.)*)

  (* (1) bool -> unit *)
  let f b = if b then () else () ;;

  (* (2) int -> int -> int.option *)
  let f a b = Some (a + b) ;;

  (* (3) (int -> int) -> int option *)
  (* This is the answer in the textbook. However, when I typed it into utop
     it gives the following error:
     Error: This expression has type int
         This is not a function; it cannot be applied.
  *)
  let f g = Some (1 + g 3) ;;

  (* (4) 'a -> ('a -> 'b) -> 'b *)
  let f x g = g x ;;

  (* (5) 'a option list -> 'b option list -> 'a * 'b list *)
  let f [Some x] [Some y] = [(x, y)] ;;

(* =========================================================================*)
(* Problem 2. [10 points] For each of the following function definitions, 
   give a typing for the function that provides its most general type (as 
   would be inferred by OCaml) or explain briefly why no type exists for the 
   function. (The first problem is done for you as an example.) *)

  (* (1) *)
  let f1 x = x +. 42. ;;
  (* f1 : float -> float *)

  (* (2) *)
  let f2 x y =
    match x with
    | (w, z) -> if w then y z else w ;;
  (* x is a pair containing w and z. w is a bool. The function returns a bool
     as well. However, z is not specified, so we can make it a polymorphic type.
     We also notice the expression of "y z". This means y is a function that
     takes in z and returns a bool. so y is of ('a -> bool).

    f2 : (bool * 'a) -> ('a -> bool) -> bool *)

  (* (3) *)
  let rec f3 x =
    match x with
    | [] -> f3
    | h :: t -> raise Exit ;;

    (* x is a list. When I type in utop it gives me errors: 
       Error: This expression has type 'a list -> 'b
          but an expression was expected of type 'b
          The type variable 'b occurs inside 'a list -> 'b *)

  (* (4) *)
  let f4 x = 
    if x then (x, true)
    else (true, not x) ;;
  
  (* x is a bool, the function returns a pair of bool
     f4: bool -> bool * bool *)

(* =========================================================================*)

  (* Problem 3. [4 points] Provide a more succinct definition of the function 
     f4 from Problem 2(4), which defines f4 to have the same type and behavior. 
  *)

  let f4 x = (x = true, true) ;;


(* ========================================================================= *)

  (* Problem 4. [8 points] The following code was intended to define a data 
     type ’a tree for a kind of polymorphic binary trees with values stored 
     at some of the leaves and with other leaves left empty, plus a function 
     sum_tree : int tree -> int that returns the sum of all the integers stored 
     in an integer tree. However, it contains errors that will generate error 
     messages and warnings in several places. Identify as many such errors and 
     warnings as there are (but no more), giving line numbers for each and 
     explaining what each problem is as specifically as you can. *)

  (* Provided code: *)
  let 'a tree = 
  | Empty
  | Leaf of 'a
  | Node of (tree, tree) ;;

  (* You should: *)
  type 'a tree =
  | Empty
  | Leaf of 'a
  | Node of 'a tree * 'a tree ;;
  (* remember, you are defining the type! 
  So a pair should be x * x not (x, x) *)

  (* Provided code: *)
  let sum_tree (t : int tree) =
    match t with
    | Leaf x -> x
    | Node (l, r) -> (sum_tree l) + (sum_tree r) ;;

  (* You should: *)
  let rec sum_tree (t : int tree) : int =
    match t with
    | Empty -> 0
    | Leaf x -> x
    | Node (l, r) -> (sum_tree l) + (sum_tree r) ;;

  (* Line1. The type definition should be introduced by type
     instead of let.
     Line4. The comma (a value constructor) should be replaced 
     by the appropriate type constructor.
     Line4. The recursive references to the type being defined 
     need to mention the argument type ’a (twice).
     Line6. The definition is intended to be recursive, but the 
     rec key-word is missing after the let.
     Lines7–8. There is no pattern for Emptytrees,so the code
     would generate a nonexhaustive match warning.*)

(* ========================================================================= *)

  (* Problem 5. [6 points] Write the correct type definition for the 
     ’a tree data type from the previous problem. Make sure it is 
     consistent with how the type is used in the walk function below. *)
  
  type tree 'a tree =
  | Empty
  | Leaf of 'a
  | Node of 'a tree * 'a tree ;;


(* ========================================================================= *)

  (* Problem 6. [4 points] What type would OCaml infer for the walk function as 
     defined? Write the type as a single OCaml type expression. *)

  (* Consider the following function named walk that “walks” over a tree and 
     applies a binary function (the argument f) to the children of a node in 
     the tree – a kind of “fold” operation for trees. At the leaves, it returns 
     whatever value is stored there, and for empty nodes it returns a default 
     value (the argument default).
  *)

    let rec walk f default (t : 'a tree) =
      match t with
      | Empty -> default
      | Leaf x -> x
      | Node (l, r) -> f (walk f default l) (walk f default r) ;;
    
    (* walk : (’a -> ’a -> ’a) -> ’a -> ’a tree -> ’a 
       Here, ('a -> 'a -> 'a) is the type of f
       'a is the type of default
       and 'a is the type of x *)

(* ========================================================================= *)

  (* Problem 7. [6 points] Notice that each time walk is recursively called, it
     passes along the same first two arguments. Write a version of walk that 
     uses a local function to avoid this redundancy. *)
    
    let walk f default (t : 'a tree) =
      let rec walk' t =
      match t with
      | Empty -> default
      | Leaf x -> x
      | Node (l, r) -> f (walk' l) (walk' r) in
    walk' t ;;

(* ========================================================================= *)

  (* Problem 8. [4 points] Use the walk function in writing a definition for a 
     function sum_tree : int tree -> int that sums up the values stored at all 
     of the leaves of an int tree. *)
    
    let sum_tree = walk (+) 0 ;;
  
  (* walk takes in a function, here it is (+), and a default value, here it is
     0, and use partial application on 'a tree *)

(* ========================================================================= *)

  (* Problem 9. [4 points] Use the walk function in writing a definition for a 
     function max_tree : int tree -> int that returns the maximum value stored 
     at the leaves of a tree. You’ll want to think carefully about what integer
     max_tree should return for the Empty tree that makes your code as simple 
     as possible. *)

    let max_tree = walk max min_int ;;

(* ========================================================================= *)

  (* A good candidate for an abstract data type is the interval. Abstractly 
     speaking, an interval is a region between two points, where all that is 
     required of points is that we be able to compare them as an ordering (so 
     that we have a well-defined notion of “between”). That is, points ought to 
     obey the following signature, which may look familiar, as you’ve seen it 
     in other contexts: *)
  
    module type COMPARABLE =
      sig
        type t
        type order = Less | Equal | Greater
        val compare : t -> t -> order
      end ;;
  
  (* Natural operations over intervals are the construction of an interval 
     between two points, the extraction of the endpoints of an interval, taking 
     the union of two intervals (the smallest interval containing both), and 
     determining the relation between two intervals. Here is a signature that 
     provides for this functionality. *)

    module type INTERVAL =
      sig
        type point
        type interval
        type relation = Disjoint | Overlaps | Contains
        (* Returns the interval between two points. *)
        val interval : point -> point -> interval
        (* Returns the endpoints of an interval as a pair with the first point
           less than the second. *)
        val endpoints : interval -> point * point
        (* Returns the union of two intervals *)
        val union : interval -> interval -> interval
        (* Returns the relation holding between two intervals *)
        val relation : interval -> interval -> relation
      end ;;

(* ========================================================================= *)

(* Problem 10. [6 points] Fill in the box to complete the skeleton of a functor 
   named MakeInterval for generating implementations of the INTERVAL signature 
   based on modules satisfying the COMPARABLE signature. (We’ve purposefully left 
   the implementation out.) *)

   module MakeInterval (Point : COMPARABLE)
            : (INTERVAL with type point = Point.t) =

(* ========================================================================= *)

(* Problem 11. [8 points] An appropriate module satisfying COMPARABLE for the 
   purpose of gener- ating discrete time intervals would be one where the type 
   is int, with an appropriate comparison function. Define a module named 
   DiscreteTime satisfying COMPARABLE where the type is int. Make sure the type 
   is accessible outside the module. *)

  module DiscreteTime : (COMPARABLE with type t = int) =
    struct
      type t = int
      type order = Less | Equal | Greater
      let compare (t1 : int) (t2 : int) : order =
        if t1 < t2 then Less
        else if t1 = t2 then Equal
        else Greater
    end ;;

  (* Solution is: *)

  module DiscreteTime : (COMPARABLE with type t = int) =
    struct
      type t = int
      type order = Less | Equal | Greater
      let compare x y = if x < y then Less
                        else if x = y then Equal
                        else Greater
    end ;;

(* ========================================================================= *)

(* Problem 12. [3 points] Now use the functor MakeInterval to define a module 
   DiscreteTimeInterval that provides interval functionality over discrete times 
   as defined by the module DiscreteTime above. *)

  module DiscreteTimeInterval =
    MakeInterval (DiscreteTime) ;;

(* ========================================================================= *)

(* Problem 13. [6 points] The intersection of two intervals is only well-defined 
   if the intervals are not disjoint. Assume that the DiscreteTimeInterval module 
   has been opened (as we’ve already done for you in the box below), allowing 
   you to make use of everything in its signature. Now, define a function 
   intersection : interval -> interval -> interval option that takes two 
   intervals and returns None if they are disjoint and otherwise returns their 
   intersection (embedded appropriately in the option type). *)

   open DiscreteTimeInterval ;;

   let intersection i j =
    if relation i j = Disjoint then None
    else let (x , y), (x', y') = endpoints i, endpoints j in
      Some (interval (max x x') (min y y')) ;;

(* ========================================================================= *)

(* Problem 14. [8 points] Provide three different unit tests that would be useful 
   in testing the correctness of the DiscreteTimeInterval module. We’ve provided 
   some context in which your tests should appear. You can use CS51.verify or 
   assert as you prefer. *)

   open CS51 ;;

   let test () =
    let open DiscreteTimeInterval in

     let i1 = interval 1 3 in
     let i2 = interval 2 6 in
     let i3 = interval 0 7 in
     let i4 = interval 4 5 in
     unit_test (relation i1 i4 = Disjoint) "disjoint\ n";
     unit_test (relation i1 i2 = Overlaps) "overlaps\ n";
     unit_test (relation i1 i3 = Contains) "contains\ n";
     unit_test (relation (union i1 i2) i4 = Contains) "unioncontains\ n";
     let i23 = intersection i1 i2 in
     unit_test (let Some e23 = i23 in endpoints e23 = (2, 3)) "intersection";;
   print_endline "tests completed" ;;

   

