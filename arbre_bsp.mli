open Graphics

type label = {
  coord : int ;
  colored : bool;
}

type bsp =
R of color option
  | L of label * bsp * bsp

(* Génère aléatoirement une configuration finale en bsp
   avec dimensions fenetre et la profondeur maximale du bsp en parametre*)
val random_bsp : int -> int -> int -> bsp

(* Renvoie la liste des rectangles en contact avec une ligne *)
val rectangles_from_line : bsp -> bool -> bsp list

(* Renvoie la couleur d'une ligne à partir de ses rectangles selon
les contraintes du jeu Mondrian*)
val line_color : bsp -> bool -> color
