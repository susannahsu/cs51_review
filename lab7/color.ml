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

type color = int * int * int ;;

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
   r, g, b ;;


(* red c / green c / blue c -- Returns the corresponding channel value
   for the color `c` *)
let red (r, _g, _b : color) : int = r ;;

let green (_r, g, b : color) : int = g ;;

let blue (_r, _g, b : color) : int = b ;;

(* color_named name -- Returns the color corresponding to the color
   `name` *)
let color_named (name : color_name) : color =
  match name with
  | Red    -> to_color 255   0   0
  | Green  -> to_color   0 255   0
  | Blue   -> to_color   0   0 255
  | Orange -> to_color 255 165   0
  | Yellow -> to_color 255 255   0
  | Indigo -> to_color  75   0 130
  | Violet -> to_color 240 130 240 ;;;;

