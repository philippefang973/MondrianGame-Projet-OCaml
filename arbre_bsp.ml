open Graphics

type label = {
  coord : int ;
  colored : bool;
}

type bsp =
R of color option
  | L of label * bsp * bsp

let size = 20 (* Dimension minimale d'un rectangle *)

(* renvoie un entier aleatoire entre mn et mx *)
let random_int mn mx=
  Random.self_init();
  (Random.int (mx+1-mn)) + mn
;;

let random_bsp width height depth_max =
  assert(depth_max>=0);
  (* depth: profondeur généré, parite: type de ligne,
     xmin et xmax: bornes de generation coord (lignes verticales)
     ymin et ymax: bornes de generation coord (lignes horizontales)
  *)
let rec random_noeud depth parite xmin xmax ymin ymax =
  if depth=0 || xmin>xmax || ymin>ymax then if (Random.bool()) then
      R(Some (rgb 0 0 150)) else R(Some (rgb 150 0 0))
  else if parite then
    let v = (random_int xmin xmax) in
    L({coord=v;colored=(not (Random.bool()) || Random.bool())},
      random_noeud (Random.int depth) (not parite) xmin (v-size) ymin ymax,
      random_noeud (Random.int depth) (not parite) (v+size) xmax ymin ymax)
  else
    let v = (random_int ymin ymax) in
    L({coord=v;colored=(not (Random.bool()) || Random.bool())},
      random_noeud (Random.int depth) (not parite) xmin xmax ymin (v-size),
      random_noeud (Random.int depth) (not parite) xmin xmax (v+size) ymax)
in random_noeud depth_max true size (width-size) size (height-size)
;;

let rectangles_from_line bsp parite =
  (*bsp_tmp: noeud courant, parite: type de ce noeud,
    b: compter les rectangles de fils gauche ou de fils droit
  *)
  let rec aux bsp_tmp parite_tmp b =
    match bsp_tmp with
      R(a) -> [R(a)]
    | L(v,g,d) ->
      if parite_tmp then
        if b then (aux d (not parite_tmp) b)
        else(aux g (not parite_tmp) b)
      else (aux g (not parite_tmp) b)@(aux d (not parite_tmp) b)
  in match bsp with
    L(v,g,d) ->
    if parite then (aux g (not parite) true)@(aux d (not parite) false)
    else  (aux g parite true)@(aux d parite false)
  | _ -> [bsp]

let line_color line parite=
  (* count_blue: nombre de rect. bleus, count_red: nombre de rect. rouges
  l: listes des rectangles en contact de la ligne
  *)
  let rec aux count_blue count_red l =
    match l with
      [] ->
      if count_blue>count_red then rgb 0 0 255
      else if count_blue<count_red then rgb 255 0 0
      else rgb 200 0 200
    | R(Some(a))::v ->
    if a = rgb 0 0 150 then aux (count_blue+1) count_red v
    else aux count_blue (count_red+1) v
    | _::v -> aux count_blue count_red v
  in match line with
    L(v,g,d) ->
    if not (v.colored) then black
    else let rectangles = (rectangles_from_line line parite) in aux 0 0 rectangles
  | _ -> failwith "L type required"
