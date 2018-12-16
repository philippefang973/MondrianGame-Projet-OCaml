open Arbre_bsp
open Sat_solver

(* desssine la configuration courante*)
val draw_current_bsp: bsp -> bsp -> unit

(* initialise la configuration initiale sans la couleur des rectangles*)
val config_initial: bsp -> bsp

(* verifie si la configuration est correcte *)
val check_current : bsp -> bsp -> bool

(* verifie si tous les rectangles sont coloriés *)
val is_full : bsp -> bool

(* colorie un rectangle *)
val click_set_color : bsp -> int -> int -> bsp
