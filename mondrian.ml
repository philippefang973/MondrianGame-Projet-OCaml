open Arbre_bsp
open Graphics


let draw_current_bsp bsp config_initial =
  let rec aux tmp_courant tmp_initial parite xmin xmax ymin ymax =
    match tmp_courant,tmp_initial with
      L({coord=c;colored=b},g,d),L(l,g',d') ->
      if parite then
        begin
        aux g g' (not parite) xmin c ymin ymax;
        aux d d' (not parite) c xmax ymin ymax;
        set_color (line_color tmp_initial parite);
        moveto c ymin;
        lineto c ymax;
        end
      else
        begin
        aux g g' (not parite) xmin xmax ymin c;
        aux d d' (not parite) xmin xmax c ymax;
        set_color (line_color tmp_initial parite);
        moveto xmin c;
        lineto xmax c;
        end
    | R(Some c),_ ->
    begin
      set_color c;
      fill_rect xmin ymin (xmax-xmin) (ymax-ymin)
      end
    | _ -> ()
  in aux bsp config_initial true 0 (size_x()-1) 0 (size_y()-1)

let rec init_config_initial config_final =
  match config_final with
    R(a) -> R(None)
  | L(l,g,d) -> L(l,init_config_initial g,init_config_initial d)


let main () =
  let config_final = random_bsp 700 700 8  in
  let config_initial = init_config_initial config_final in
  open_graph " 700x700";
  set_window_title "Mondrian";
  set_line_width 3;
  draw_current_bsp config_final config_final;
  wait_next_event [Button_down];
;;

let _ = main ()
;;
