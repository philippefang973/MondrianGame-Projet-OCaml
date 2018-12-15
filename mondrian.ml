open Arbre_bsp
open Graphics

type rectangle = {
  x : int;
  y : int;
  width : int;
  height : int;
}

type rect_list = rectangle list

let config_final = random_bsp 700 700 8

let draw_current_bsp bsp =
  let rec aux tmp_courant tmp_final parite xmin xmax ymin ymax =
    match tmp_courant,tmp_final with
      L({coord=c;colored=b},g,d),L(l,g',d') ->
      if parite then
        begin
        aux g g' (not parite) xmin c ymin ymax;
        aux d d' (not parite) c xmax ymin ymax;
        set_color (line_color tmp_final parite);
        moveto c ymin;
        lineto c ymax;
        end
      else
        begin
        aux g g' (not parite) xmin xmax ymin c;
        aux d d' (not parite) xmin xmax c ymax;
        set_color (line_color tmp_final parite);
        moveto xmin c;
        lineto xmax c;
        end
    | R(Some c),_ ->
    begin
      set_color c;
      fill_rect xmin ymin (xmax-xmin) (ymax-ymin);
      end
    | _ -> ()
  in aux bsp config_final true 0 (size_x()-1) 0 (size_y()-1)

let config_initial =
  let rec aux bsp =
  match bsp with
    R(a) -> R(None)
  | L(l,g,d) -> L(l,aux g,aux d)
  in aux config_final

let check_current bsp =
  let rec aux tmp_courant tmp_final parite=
    match tmp_courant,tmp_final with
      L(l,g,d),L(l',g',d') ->
      let couleur1 = line_color tmp_courant parite in
      let couleur2 = line_color tmp_courant parite in
      if couleur1<>couleur2 then false
      else (aux g g' (not parite)) && (aux d d' (not parite))
    | _ -> true
  in aux bsp config_final true

let rec is_full bsp =
  match bsp with
    L(l,g,d) -> (is_full g) && (is_full d)
  | R(None) -> false
  | _ -> true

let main () =
  let config_user = config_initial in
  open_graph " 700x700";
  set_window_title "Mondrian";
  set_line_width 3;
  while not (is_full config_user) do
    draw_current_bsp config_user;
    let e = wait_next_event [Button_down] in
    let x = e.mouse_x and y = e.mouse_y in
    print_endline ((string_of_int x)^" "^(string_of_int y));
  done
;;

let _ = main ()
;;
