open Graphics

type label = {
  coord : int ;
  colored : bool;
}

type bsp =
R of color option
  | L of label * bsp * bsp

let size = 20 (* Dimension minimale d'un rectangle *)

let purple_line = rgb 200 0 200
let red_line = rgb 255 0 0
let blue_line = rgb 0 0 255
let red_rect = rgb 150 0 0
let blue_rect = rgb 0 0 150

(* renvoie un entier aleatoire entre mn et mx *)
let random_int mn mx=
  Random.self_init();
  (Random.int (mx+1-mn)) + mn
;;

let random_bsp width height depth_max =
assert(depth_max>=0);
let rec random_noeud depth parite xmin xmax ymin ymax =
  if depth=0 || xmin>xmax || ymin>ymax then if (Random.bool()) then
      R(Some blue_rect) else R(Some red_rect)
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
  let rec aux bsp_tmp parite_tmp rleaf father fleaf=
    match bsp_tmp with
      R(a) -> [(a,father,fleaf)]
    | L(l,g,d) ->
      if parite_tmp then
        if rleaf then (aux d (not parite_tmp) rleaf l.coord false)
        else (aux g (not parite_tmp) rleaf l.coord true)
      else (aux g (not parite_tmp) rleaf l.coord true)
           @(aux d (not parite_tmp) rleaf l.coord false)
  in match bsp with
    L(l,g,d) ->
    if parite then (aux g (not parite) true l.coord true)
                   @(aux d (not parite) false l.coord false)
    else  (aux g parite true l.coord true)@(aux d parite false l.coord false)
  | _ -> failwith "L type required"

let line_color line parite =
  let rec aux count_blue count_red l =
    match l with
      ((Some a),_,_)::v ->
    if a = blue_rect then aux (count_blue+1) count_red v
    else aux count_blue (count_red+1) v
    | _ ->
      if count_blue>count_red then blue_line
      else if count_blue<count_red then red_line
      else purple_line
  in match line with
    L(v,g,d) ->
    if not (v.colored) then black
    else let rectangles = (rectangles_from_line line parite) in aux 0 0 rectangles
  | _ -> failwith "L type required"
