(*
                              CS51 Lab 7
                          Modules & Functors

                 A module for colors and color names

The representation for colors in this implementation of the signature
in `color.mli` is really obscure (and arguably unnecessarily so). By
the way, it also has some bugs so it doesn't pass all the unit
tests. No need to debug it though, or even read it. You'll be
replacing it wholesale with your own hopefully simpler
implementation. *)

type color = ;;

type color_name =
  | Red
  | Green
  | Blue
  | Orange
  | Yellow
  | Indigo
  | Violet ;;

(* to_color r g b -- Returns the `color` corresponding to the RGB values
   given by `r`, `g`, and `b` *)
let to_color (r : int) (g : int) (b : int) : color =


(* red c / green c / blue c -- Returns the corresponding channel value
   for the color `c` *)
let red : int = ;;

let green : int = ;;

let blue : int = ;;

(* color_named name -- Returns the color corresponding to the color
   `name` *)
let color_named (name : color_name) : color =
 ;;
