open Arbre_bsp
open Sat_solver
open Graphics
   
module Variables = struct
  type t = int * bool
  let compare x y =
    let (x1,x2) = x and (y1,y2) = y in
    if x1>y1 then 1
    else if x1<y1 then -1
    else if x2=y2 then 0
    else if x2=true then 1
    else -1
end
                 
module Sat = Sat_solver.Make(Variables)
           
let config_initial config_final =
  let rec aux bsp =
    match bsp with
      R(a) -> R(None)
    | L(l,g,d) -> L(l,aux g,aux d)
  in aux config_final
;;

(* verifie si tous les rectangles sont coloriés *)
let rec is_full bsp =
  match bsp with
    L(l,g,d) -> (is_full g) && (is_full d)
  | R(None) -> false
  | _ -> true
;;

let check_current bsp1 bsp2 =
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
  (* les x et y delimitent la zone de tracé d'un noeud *)
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
;;

let click_set_color bsp x y=
  (* les x et y delimitent un rectangle qui contient les coordonnées du clique*)
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
         if c = red_rect then R(Some blue_rect)
         else R(None)
       else tmp
    | R(None) ->
       if xmin<=x && ymin<=y && xmax>=x && ymax>=y then R(Some red_rect)
       else tmp
  in aux bsp true 0 (size_x()-1) 20 (size_y()-1)
   
;;

let modelisation bsp1 bsp2 =
  (* liste les clauses unitaires *)
  let rec single_clauses l =
    match l with
      [] -> []
    | (Some a,f,b)::v ->
       if a=red_rect then [(true,(f,b))]::(single_clauses v)
       else [(false,(f,b))]::(single_clauses v)
    | (None,f,b)::v -> [(true,(f,b));(false,(f,b))]::(single_clauses v)
  in
  (* liste toutes les combinaisons de litteraux de même polarité possibles*)
  let rec all_clauses l pol=
    match l with
      [] -> [[]]
    | [(_,father,leaf)] -> [[(pol,(father,leaf))]]
    | (_,father,leaf)::v ->
       let res = List.map (fun x-> (pol,(father,leaf))::x) (all_clauses v pol)
       in [(pol,(father,leaf))]::(res@(all_clauses v pol))
  in
  (* renvoie la CNF=au moins n rectangles d'une couleur parmi m *)
  let at_least n m l  =
    let x = m-n+1 in
    (List.filter (fun l' -> (List.length l')=x) (all_clauses l true))
  in
  (* renvoie la CNF=au plus n rectangles d'une couleur parmi m *)
  let at_most n m l =
    let x = m-(m-n)+1 in
    (List.filter (fun l' -> (List.length l')=x) (all_clauses l false))
  in
  (* renvoie la CNF complete d'une ligne selon sa couleur *)
  let init_fnc c l =
    let m = (List.length l) in
    let k = (single_clauses l) in
    let n = m/2 in
      if c = black then k
      else if c = red_line then k@(at_least (n+1) m l)
      else if c = blue_line then
        if m mod 2 = 0 then k@(at_most (n-1) m l) else k@(at_most n m l)
      else if m mod 2 = 0 then k@((at_least n m l)@(at_most n m l))
      else k@((at_least (n+1) m l)@(at_most (n+1) m l))
  in
  let rec aux tmp1 tmp2 parite =
    match tmp1,tmp2 with
      L(l,g,d),L(l',g',d') ->
       (init_fnc (line_color tmp2 parite) (rectangles_from_line tmp1 parite))
       @((aux g g' (not parite))@(aux d d' (not parite)))
    | _ -> []
  in aux bsp1 bsp2 true
   
let extension_to_bsp solution bsp =
  (*colorie les rectangles avec la solution du sat solver*)
  let rec set_rect_color tmp x found=
    let (b,(father,leaf)) = x in
    let c = if b then red_rect else blue_rect in
    match tmp with
      R(None) -> if found then R(Some c) else tmp
    | L(l,g,d) ->
       if found then tmp
       else if l.coord = father then
         if leaf then L(l,set_rect_color g x true,d) else L(l,g,set_rect_color d x true)
       else L(l,set_rect_color g x false,set_rect_color d x false)
    | _ -> tmp
  in
  let rec aux l tmp =
    match l with
      [] -> tmp
    | a::v -> aux v (set_rect_color tmp a false)
  in aux solution bsp
;;

let main () =
  
  (* Reccueil de données*)
  print_endline "Choisir profondeur maximale bsp";
  let depth = read_int() in
  
  (* Creation partie*)
  let run = ref true and state = ref "play" in
  let jeuFin = ref (random_bsp 700 680 depth) in
  let jeuCourant = ref (config_initial !jeuFin) in
  let jeuAffiche = ref !jeuCourant in
  let extensionList = ref [] in
  open_graph (" 700x700");
  set_window_title "Mondrian";
  set_font "-*-fixed-medium-r-semicondensed--20-*-*-*-*-*-iso8859-1";
  let (u1,v1) = (text_size "Solution")
  and (u2,v2) = (text_size "Extension")
  and (u3,v3) = (text_size "Continuer")
  and (u4,v4) = (text_size "Retourner")
  and (u5,v5) = (text_size "Recommencer")
  and (u6,v6) = (text_size "Quitter") in
  
  (* boucle d'interaction *)
  while !run do
    clear_graph();
    set_line_width 3;
    moveto 0 20;
    set_color black;
    lineto (size_x()-1) 20;
    
    match !state with

      (* Affiche la configuration courante, 
         affiche les boutons Solution et Extension *)
      "play" -> (
      draw_current_bsp !jeuAffiche !jeuFin;
      synchronize();
      set_color black;
      moveto 0 0;
      draw_string "Solution  Extension";
      (* Evenements click souris *)
      let e = wait_next_event [Button_down] in
      let x = e.mouse_x and y = e.mouse_y in
      if x>=0 && x<=u1 && y>=0 && y<=v1 then (
        jeuAffiche := !jeuFin;
        state := "solution";
      )
      else if x>=u1+20 && x<=u1+u2+20 && y>=0 && y<=v2 then (
        let model = modelisation !jeuCourant !jeuFin in
        match (Sat.solve model) with
          Some a -> (state := "available"; extensionList := a)
        | None -> state := "unavailable"
      )
      else (jeuCourant := click_set_color !jeuCourant x y;
            if (is_full !jeuCourant) && (check_current !jeuCourant !jeuFin)
            then state := "win" else jeuAffiche := !jeuCourant)
    )

    (* Affiche un message indiquant l'existence d'une extension,
       Affiche les boutons Coutinuer et Retourner *)
    | "available" -> (
      set_font "-*-fixed-medium-r-semicondensed--50-*-*-*-*-*-iso8859-1";
      moveto ((size_x())/10) ((size_y())/2);
      draw_string "Extension disponible";
      set_font "-*-fixed-medium-r-semicondensed--20-*-*-*-*-*-iso8859-1";
      moveto 0 0;
      draw_string "Continuer Retourner";
      (* Evenements click souris *)
      let e = wait_next_event [Button_down] in
      let x = e.mouse_x and y = e.mouse_y in
      if x>=0 && x<=u3 && y>=0 && y<=v3 then (
        jeuAffiche := extension_to_bsp !extensionList !jeuCourant;
        state := "solution";
      )
      else if x>=u3+20 && x<=u3+u4+20 && y>=0 && y<=v4 then state := "play";
    )

    (* Affiche un message indiquant l'inexistence d'une extension,
       Affiche le bouton Retourner *)
    | "unavailable" -> (
      set_font "-*-fixed-medium-r-semicondensed--50-*-*-*-*-*-iso8859-1";
      moveto ((size_x())/10) ((size_y())/2);
      draw_string "Extension indisponible";
      set_font "-*-fixed-medium-r-semicondensed--20-*-*-*-*-*-iso8859-1";
      moveto 0 0;
      draw_string "Retourner";
      (* Evenements click souris *)
      let e = wait_next_event [Button_down] in
      let x = e.mouse_x and y = e.mouse_y in
      if x>=0 && x<=u1 && y>=0 && y<=v1 then state := "play";
    )

    (* Affiche la configuration finale,
     affiche les boutons Recommencer et Quitter*)
    | "solution" -> (
      draw_current_bsp !jeuAffiche !jeuFin;
      synchronize();
      set_color black;
      moveto 0 0;
      draw_string "Recommencer Quitter";
      (* Evenements click souris *)
      let e = wait_next_event [Button_down] in
      let x = e.mouse_x and y = e.mouse_y in
      if x>=0 && x<=u5 && y>=0 && y<=v5 then (
        jeuFin := (random_bsp 700 700 depth);
        jeuCourant := (config_initial !jeuFin);
        jeuAffiche := !jeuCourant;
        state := "play"
      )
      else if x>=u5+20 && x<=u5+u6+20 && y>=0 && y<= v6 then run := false
    )
                  
    (* Affiche un message de victoire,
       Affiche les boutons Recommencer et Quitter *)  
    | "win" -> (
      set_font "-*-fixed-medium-r-semicondensed--50-*-*-*-*-*-iso8859-1";
      moveto ((size_x())/10) ((size_y())/2);
      draw_string "Felicitations!";
      set_font "-*-fixed-medium-r-semicondensed--20-*-*-*-*-*-iso8859-1";
      moveto 0 0;
      draw_string "Recommencer Quitter";
      (* Evenements click souris *)
      let e = wait_next_event [Button_down] in
      let x = e.mouse_x and y = e.mouse_y in
      if x>=0 && x<=u5 && y>=0 && y<=v5 then (
        jeuFin := (random_bsp 700 700 depth);
        jeuCourant := (config_initial !jeuFin);
        jeuAffiche := !jeuCourant;
        state := "play"
      )
      else if x>=u5+20 && x<=u5+u6+20 && y>=0 && y<= v6 then run := false
    )
    | _ -> ()
  done;
;;

let _ = main ()
;;
