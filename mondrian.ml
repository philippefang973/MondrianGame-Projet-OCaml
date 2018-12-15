open Arbre_bsp
open Graphics

let config_initial config_final =
  let rec aux bsp =
  match bsp with
    R(a) -> R(None)
  | L(l,g,d) -> L(l,aux g,aux d)
  in aux config_final
;;

let rec is_full bsp =
    (* verifie que tous les rectangles sont coloriés *)
    match bsp with
      L(l,g,d) -> (is_full g) && (is_full d)
    | R(None) -> false
    | _ -> true
;;

let check_current bsp1 bsp2 =
  (* verifie le coloriage de la configuration courante *)
  let rec aux tmp1 tmp2 parite=
    match tmp1,tmp2 with
      L(l,g,d),L(l',g',d') ->
      let couleur1 = line_color tmp1 parite in
      let couleur2 = line_color tmp2 parite in
      if couleur1<>couleur2 then false
      else (aux g g' (not parite)) && (aux d d' (not parite))
    | _ -> true
  in aux bsp1 bsp2 true
;;

let draw_current_bsp bsp1 bsp2 =
  let rec aux tmp1 tmp2 parite xmin xmax ymin ymax =
    match tmp1,tmp2 with
      L({coord=c;colored=b},g,d),L(l,g',d') ->
      if parite then
        begin
          aux g g' (not parite) xmin c ymin ymax;
          aux d d' (not parite) c xmax ymin ymax;
          set_color (line_color tmp2 parite);
          moveto c ymin;
          lineto c ymax;
        end
      else
        begin
          aux g g' (not parite) xmin xmax ymin c;
          aux d d' (not parite) xmin xmax c ymax;
          set_color (line_color tmp2 parite);
          moveto xmin c;
          lineto xmax c;
        end
    | R(Some c),_ ->
      begin
        set_color c;
        fill_rect xmin ymin (xmax-xmin) (ymax-ymin);
      end
    | _ -> ()
  in aux bsp1 bsp2 true 0 (size_x()-1) 0 (size_y()-1)
;;

let click_set_color bsp x y=
  let rec aux tmp parite xmin xmax ymin ymax =
    match tmp with
      L({coord=c;colored=b},g,d)->
      if parite then
        L({coord=c;colored=b},
          aux g (not parite) xmin c ymin ymax,
          aux d (not parite) c xmax ymin ymax)
      else
      L({coord=c;colored=b},
        aux g (not parite) xmin xmax ymin c,
        aux d (not parite) xmin xmax c ymax)
    | R(Some c)->
      if xmin<=x && ymin<=y && xmax>=x && ymax>=y then
        if c = rgb 150 0 0 then R(Some(rgb 0 0 150))
        else R(None)
      else tmp
    | R(None) ->
      if xmin<=x && ymin<=y && xmax>=x && ymax>=y then R(Some(rgb 150 0 0))
      else tmp
  in aux bsp true 0 (size_x()-1) 0 (size_y()-1)

let main () =

  (* Reccueil de données*)
  print_endline "Choisir longueur fenetre:";
  let longueur = read_int() in
  print_endline "Choisir largeur fenetre:";
  let largeur = read_int() in
  print_endline "Choisir profondeur maximale bsp";
  let depth = read_int() in

  (* Creation fenetre*)
  let jeuFin = random_bsp longueur largeur depth in
  let jeuCourant = ref (config_initial jeuFin) in
  open_graph (" "^(string_of_int longueur)^"x"^(string_of_int largeur));
  set_window_title "Mondrian";

  (* boucle d'interaction *)
  while not (is_full !jeuCourant) do
    clear_graph();
    set_line_width 3;
    draw_current_bsp !jeuCourant jeuFin;
    synchronize();
    let e = wait_next_event [Button_down] in
    let x = e.mouse_x and y = e.mouse_y in
    jeuCourant := click_set_color !jeuCourant x y;
  done;
  if check_current !jeuCourant jeuFin then
  print_endline "Felicitation, c'était un bon remplissage!"
  else print_endline "Malheureusement, ce n'était pas un bon remplissage"
;;

let _ = main ()
;;
