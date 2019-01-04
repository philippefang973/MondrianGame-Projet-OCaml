open Graphics

type label = {
  coord : int ;
  colored : bool;
}

type bsp =
R of color option
  | L of label * bsp * bsp

  (* Definissent les couleurs des rectangles et des lignes*)
val purple_line : color
val red_line : color
val blue_line : color
val red_rect : color
val blue_rect : color

(* Génère aléatoirement une configuration finale en bsp
   @Param: Dimensions fenetre, profondeur maximale
   @Return: bsp généré *)
val random_bsp : int -> int -> int -> bsp

(* Renvoie la liste des rectangles en contact avec une ligne
   @Param: Ligne, parité
   @Return: liste des rectangles sous forme (couleur,père,position) *)
val rectangles_from_line : bsp -> bool -> (color option * int * bool) list

(* Renvoie la couleur d'une ligne
   @Param: ligne, parité
   @Return: couleur
*)
val line_color : bsp -> bool -> color
