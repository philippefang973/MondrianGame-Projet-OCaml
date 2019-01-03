open Graphics

type label = {
  coord : int ;
  colored : bool;
}

type bsp =
R of color option
  | L of label * bsp * bsp

val purple_line : color
val red_line : color
val blue_line : color
val red_rect : color
val blue_rect : color

(* Génère aléatoirement une configuration finale en bsp
   avec dimensions fenetre et la profondeur maximale du bsp en parametre*)
val random_bsp : int -> int -> int -> bsp

(* Renvoie la liste des rectangles en contact avec une ligne
   dans un triplet couleur, coordonnées du père et indicateur de si c'est une
   feuille gauche ou une feuille droite de son père *)
val rectangles_from_line : bsp -> bool -> (color option * int * bool) list

(* Renvoie la couleur d'une ligne *)
val line_color : bsp -> bool -> color
