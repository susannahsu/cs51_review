(* Q2 Finger exercises *)

(* Q2.1 *)
(* Provide a succinct definition of a function named add_three that returns the 
sum of three and its integer argument. *)





(* Q2.2 *)
(* Define a curried function named power that returns its first argument (an 
integer) raised to the power of its second argument (also an integer, which you 
can assume is nonnegative), returning an integer result. *)




(* Q2.3 *)
(* Now define an uncurried version of power . *)






(* ========================================================================= *)
(* Q3 Types of subexpressions in context *)

(* Consider this snippet of code, which defines and algebraic data type 
'a combine and a function f: *)

type 'a combine =
  | Combine of ('a -> 'a) * ('a combine)
  | Base of ('a -> 'a) * ('a option) ;;

let rec f x a =
  match x with
  | Base (f, None) -> f a
  | Base (f, Some x) -> f x
  | Combine (g, r) -> f r (g a) ;;

(* 
f a -> a
f -> b
f -> c
f r -> d
(g a) -> e
*)

(* For each of the subexpressions in the labeled boxes, specify the type of the 
subexpression in the context in which it appears. If the boxed element is not 
a single subexpression (and therefore has no type), answer "no type". *)


(* Q3.1 *)
(* What is the type of the expression f a in the box labeled a ? *)



(* Q3.2 *)
(* What is the type of the expression f in the box labeled b ? *)


(* Q3.3 *)
(* What is the type of the expression f in the box labeled c? *)


(* Q3.4 *)
(* What is the type of the expression f r in the box labeled d ? *)



(* Q3.5 *)
(* What is the type of the expression (g a) in the box labeled e? *)



(* ========================================================================= *)

(* Q4 Short answer questions *)
(* 
Each of the code snippets below has a blank in it, marked with a long 
underline. Your job is to find a single OCaml pattern or expression that can 
fill the blank such that the final expression evaluates to 42 . 

If no such expression exists, select "no such expression". If an expression 
does exist, select "this expression" and provide the expression in the answer 
box.


(Note that the expressions may reference and make use of earlier definitions 
and functions in the exam.)

*)

(* For example, if we provide the snippet *)
let f x = x + 3 ;;
(* f ____________________ ;; *)

(* 
you would check the "this expression" option and provide an answer such as 
39
or
3 * 13
or any of a wide variety of other expressions.
*)

(* Q4.1 *)
let setup = ___________ in
match setup with
| [] -> 21
| hd :: _tl -> hd ;;

(* no such expression or this expression *)


(* Q4.2 *)
let setup pair = ___________ in
let (x, y) = setup (14, 42) in
x + y + setup (14, 0) ;;

(* no such expression or this expression*)


(* Q4.3 *)
let setup = ___________ in
setup (setup 21) ;;

(* no such expression or this expression *)



(* Q4.4 *)
type various = {first : int; second : bool} ;;

let setup = _____________ ;;

let {first; second} = setup in
first + if not second then ~- first
        else first * first ;;

(* no such expression this expression *)

(* ========================================================================= *)

(* Q5 High-order functional programming *)
(* 
The following problems ask you to define simple functions. Each solution should 
nontrivially use one of the List module higher- order functions for mapping, 
folding, and filtering exactly once to implement the function (in addition to 
constructs from the base OCaml language). You may assume that the List module 
has already been opened.
*)


(* Q5.1 *)
(*
The tower function takes a list of integers
[a1; a2; a3; ...; an] and returns their nested exponentiation
*)

(* For example,
# tower [2; 2] ;;
- : int = 4
# tower [2; 3] ;;
- : int = 8
# tower [2; 3; 2] ;;
- : int = 512
*)

(*
(Notice that the last example computes 2^(3^2) = 512 and not (2^3)^2 = 64.)

Implement tower (using map/fold/filter as specified above). You may assume that
that all of the exponents are nonnegative. You may assume availability of a 
correct implementation of the power function as described in Question 2.2 as 
well. Your implementation may handle the empty list however you see fit.

Hint: a^1 = a
*)

(* Q5.2 *)
(* 
The List.find function is described as follows in the List module documentation:

val find : ('a -> bool) -> 'a list -> 'a
find p l returns the first element of the list l that satisfies the predicate p
Raises Not_found if there is no value that satisfies p in the list l

It has the following behavior:

# find ((=) 4) [1; 3; 5; 7] ;;
Exception: Not_found.
# find ((=) 4) [1; 3; 4; 7] ;;
- : int = 4
# find ((<) 4) [1; 3; 5; 1] ;;
- : int = 5
*)

(* Implement find (using only one map/fold/filter as specified above). *)


(* ========================================================================= *)
(* Q6 Circuits of resistors *)

(* 
This problem concerns circuits of electrical resistors, but you don't need to 
know anything about the topic; we'll cover everything you need to know about 
resistors here.

In an electrical circuit, a resistor is a component that impedes the flow of 
electricity by a certain amount, called its resistance, and measured in ohms. 
You can see some in the picture above. We depict a resistor whose resistance is 
R ohms with a graphic like the one in Figure 1(a).

For the rest of information see the exam write up.
*)

(* Q6.1 *)
(* 
Define an algebraic data type circuit for circuits that can be composed of a 
single resistor (with its resistance given as a float ) or composed of a pair 
of circuits connected either in series or in parallel.
*)

(* Q6.2 *)
(* 
Define a value circ_a of type circuit that represents a single resistor of 3 ohms.
*)


(* Q6.3 *)
(* 
Define a value circ_c of type circuit that represents the circuit in Figure 1(c).
*)


(* Q6.4 *)
(* 
Define a value circ_d of type circuit that represents the circuit in Figure 1(d).
*)


(* Q6.5 *)
(* 
Define a function resistance : circuit -> float that returns the total 
resistance of its circuit argument.
*)




(* ========================================================================= *)
(* Q7 A functor for finite sequences *)

(*
We'll define a finite sequence as a series of elements, finite in number, all 
of a single type starting with some initial element and with each succeeding 
element being generated from the previous one by a next function. For instance, 
here is a length-5 sequence of natural numbers (implemented in OCaml as an int list ):

[0; 1; 2; 3; 4]

The initial element is 0 and the generating next function is the
successor function.

*)

(* A signature for modules that provide a finite sequence is as follows: *)
module type SEQUENCE =
  sig
    type t
    val sequence : int -> t list
  end

(* 
The signature requires that the module have a type t of the elements of the 
sequence and a function sequence that, given an integer length generates a 
sequence of that length as a list of elements of type t.
*)

(* 
Here is a (partial) implementation of a module Natnums for sequences of natural 
numbers. (In addition to defining t and sequence as called for in the SEQUENCE 
module signature, it also defines a function sequence_from that is useful as 
an auxiliary function in defining sequence .)
*)

module Natnums : ________________ (a) =
  struct
    type t = _____________ (b)
    let rec sequence_from from length =
      if length = 0 then []
      else from :: sequence_from (succ from) (length - 1)
    let sequence length = _____________ (c)
  end;;

(* 
With this module, we should be able to generate behavior like:

# Natnums.sequence 5 ;;
- : int list = [0; 1; 2; 3; 4]
# Natnums.sequence 10 ;;
- : int list = [0; 1; 2; 3; 4; 5; 6; 7; 8; 9]
*)

(* ========================================================================= *)
(* Q7.1 *)
(* 
We've left out a few things in the implementation of the Natnums module, marked 
with the red labeled boxes.

What belongs in the box labeled a?
*)





(* ========================================================================= *)
(* Q7.2 *)
(* 
What belongs in the box labeled b?
*)





(* ========================================================================= *)
(* Q7.3 *)
(* 
What belongs in the box labeled c?
*)






(* ========================================================================= *)
(* Q7.4 *)
(* 
Similarly, here is a (partial) implementation of a module Diminishing for a 
sequence where each float element is half of the preceding one:
*)

module Diminishing : _______________ (d) =
  struct
    type t = _____________ (e)
    let rec sequence_from from length =
      if length = 0 then []
      else from :: sequence_from _________ (f) (length - 1)
    let sequence length = __________ (g)
  end ;;

(* For instance: 
# Diminishing.sequence 3 ;;
- : Diminishing.t list = [1.; 0.5; 0.25]
# Diminishing.sequence 5 ;;
- : Diminishing.t list = [1.; 0.5; 0.25; 0.125; 0.0625]
*)

(* 
Again, we've left out a few things in the implementation of the Diminishing 
module, marked with the red labeled boxes.
*)

(* What belongs in the box labeled d? *)




(* ========================================================================= *)
(* Q7.5 *)
(* What belongs in the box labeled e? *)





(* ========================================================================= *)
(* Q7.6 *)
(* What belongs in the box labeled f? *)



(* ========================================================================= *)
(* Q7.7 *)
(* What belongs in the box labeled g? *)



(* ========================================================================= *)






(* ========================================================================= *)






(* ========================================================================= *)






(* ========================================================================= *)





(* ========================================================================= *)






(* ========================================================================= *)







(* ========================================================================= *)





(* ========================================================================= *)






(* ========================================================================= *)







(* ========================================================================= *)







(* ========================================================================= *)






(* ========================================================================= *)






(* ========================================================================= *)





(* ========================================================================= *)





(* ========================================================================= *)






(* ========================================================================= *)






(* ========================================================================= *)






(* ========================================================================= *)






(* ========================================================================= *)






(* ========================================================================= *)







(* ========================================================================= *)






(* ========================================================================= *)







