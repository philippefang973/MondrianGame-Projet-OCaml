open Arbre_bsp

(* initialise la configuration initiale sans la couleur des rectangles
   @Param: configuration finale
   @Return: configuration initiale
*)
val config_initial: bsp -> bsp

(* verifie si tous les rectangles sont coloriés
   @Param: configuration courante
   @Return: vrai si tout est colorié faux sinon
*)
val is_full : bsp -> bool

(* verifie si la configuration est correctement coloriée
   @Param: configuration courante, configuration finale
   @Return: vrai si toutes couleurs respectent les contraintes faux sinon
*)
val check_current : bsp -> bsp -> bool

(* desssine la configuration courante
   @Param: configuration courante, configuration finale
*)
val draw_current_bsp: bsp -> bsp -> unit

(* colorie un rectangle
   @Param:configuration courante et coordonnée du clique souris
   @Return: nouvelle configuration courante
*)
val click_set_color : bsp -> int -> int -> bsp

(* genere la CNF correspondant à la configuration courante pour le Sat Solver
   @Param: configuration courante, configuration finale
   @Return: liste de liste de variables de type t
*)
val modelisation : bsp -> bsp -> (bool*(int*bool)) list list

(* met à jour la configuration courante avec l'extension trouvée
   @Param: liste de variables de type t, configuration courante
   @Return: nouvelle configuration courante
*)
val extension_to_bsp : (bool*(int*bool)) list -> bsp -> bsp
