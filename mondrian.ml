open Arbre_bsp
open Sat_solver
open Graphics

module Variables = struct
  type t = color * bsp
  let compare c1 c2 = if c1 > c2 then 1 else if c1 = c2 then 0 else -1
end

module Sat = Sat_solver.Make(Variables)

let fnc_rect_color list_rect =
  let rec at_least tmp res =
      match tmp with
        [] -> res
        | a::v -> at_least v ([(true,(rgb 150 0 0,a));(true,(rgb 0 0 150,a))]::res)
    in
  let rec at_most tmp res =
    match tmp with
      [] -> res
    | a::v -> at_most v ([(false,(rgb 150 0 0,a));(false,(rgb 0 0 150,a))]::res)
    in (at_least list_rect [])@(at_most list_rect [])

let fnc_line_color list_rect line parite red blue =
  match line with R(_) -> failwith "Line required"
  | L(l,_,_) ->
    let c = line_color line parite in
    let fnc_rect = fnc_rect_color list_rect in
    if not l.colored then [(true,(c,line))]::fnc_rect
    else fnc_rect

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
      if couleur1<>couleur2 && l.colored=true then false
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
  in aux bsp1 bsp2 true 0 (size_x()-1) 20 (size_y()-1);
  moveto 0 20;
  set_color black;
  lineto (size_x()-1) 20
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
  in aux bsp true 0 (size_x()-1) 20 (size_y()-1)

;;

let main () =

  (* Reccueil de données*)
  print_endline "Choisir profondeur maximale bsp";
  let depth = read_int() in

  (* Creation fenetre*)
  let jeuFin = ref (random_bsp 700 700 depth) in
  let jeuCourant = ref (config_initial !jeuFin) in
  let jeuAffiche = ref !jeuCourant in
  open_graph (" 700x700");
  set_window_title "Mondrian";
  set_font "-*-fixed-medium-r-semicondensed--20-*-*-*-*-*-iso8859-1";
  let run = ref true and canPlay= ref true in
  let (u1,v1) = (text_size "Solution")
  and (u2,v2) = (text_size "Extension")
  and (u3,v3) = (text_size "Quitter")
  and (u4,v4) = (text_size "Recommencer") in

  while !run do
  (* boucle d'interaction *)
    while !run && not ((is_full !jeuCourant) && (check_current !jeuCourant !jeuFin)) do
      (* Afficher jeu*)
    clear_graph();
    set_line_width 3;
    draw_current_bsp !jeuAffiche !jeuFin;
    synchronize();

    (* Afficher options *)
    moveto 0 0;
    if !canPlay then
      begin
      draw_string "Solution";
      moveto ((current_x())+20) 0;
      draw_string "Extension";
      moveto ((current_x())+20) 0;
      end
    else
      begin
      draw_string "Recommencer";
      moveto ((current_x())+20) 0;
      end;
    draw_string "Quitter";

    (* Attendre click souris*)
    let e = wait_next_event [Button_down] in
    let x = e.mouse_x and y = e.mouse_y in

    if !canPlay then
      if x>=0 && x<=u1 && y>=0 && y<=20
      then begin
        jeuAffiche := !jeuFin;
        canPlay := false
      end
      else if x>=(u1+20) && x<=(u1+u2+20) && y>=0 && y<=20
      then canPlay := false
      else if x>=(u1+u2+20) && x<=(u1+u2+u3+20) && y>=0 && y<=20
      then run := false
      else
        begin
          jeuCourant := click_set_color !jeuCourant x y;
          jeuAffiche := !jeuCourant
        end
    else if x>=0 && x<=u4 && y>=0 && y<=20
    then
      begin
        jeuFin := (random_bsp 700 700 depth);
        jeuCourant := (config_initial !jeuFin);
        jeuAffiche := !jeuCourant;
        canPlay := true
      end
    else if x>=(u4+20) && x<=(u3+u4+20) && y>=0 && y<=20
    then run:=false
    done;

    (* Afficher fin du jeu*)
    if !run then
      begin
        clear_graph();
        set_font "-*-fixed-medium-r-semicondensed--50-*-*-*-*-*-iso8859-1";
        moveto ((size_x())/10) ((size_y())/2);
        draw_string "Felicitations!";
        set_font "-*-fixed-medium-r-semicondensed--20-*-*-*-*-*-iso8859-1";
        moveto 0 0;
        draw_string "Recommencer";
        moveto ((current_x())+20) 0;
        draw_string "Quitter";
        moveto 0 0;
        let e = wait_next_event [Button_down] in
        let x = e.mouse_x and y = e.mouse_y in
        if x>=0 && x<=u4 && y>=0 && y<=20
        then
          begin
           jeuFin := (random_bsp 700 700 depth);
           jeuCourant := (config_initial !jeuFin);
           jeuAffiche := !jeuCourant;
          end
        else if x>=(u4+20) && x<=(u3+u4+20) && y>=0 && y<=20
        then run:=false
      end
  done;

;;

let _ = main ()
;;
