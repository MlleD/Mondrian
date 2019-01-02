open Graphics
open Bsp

(* Dimensions des zones de la fenetre *)
let window_sizex = 800;;
let window_sizey = 600;;
let height_message_zone = 50;;
let game_sizey = window_sizey - height_message_zone;;

(* Initialise la fenêtre graphique *)
let init_window () : unit = 
  let dims_string = " " ^ (string_of_int window_sizex) ^ "x" ^ (string_of_int window_sizey)
  in open_graph dims_string;
  set_window_title "Mondrian";
;;

(* Renvoie la couleur suivante *)
let next_color current_color = 
  if current_color = red then blue
  else if current_color = blue then white
  else red
;;

(* Trace les lignes (avec leur couleur) de la liste de lignes passée en paramètre *)
let trace_lines lines_list = 
  let aux (rect, col) = 
	Graphics.moveto rect.xmin rect.ymin;
	Graphics.set_color (Conversion.graphics_color_of_colour col Graphics.black);
  Graphics.lineto rect.xmax rect.ymax
  in List.iter aux lines_list
;;
  
(* Fonction affichant, sur le canevas graphique, la configuration courante du joueur. *)
let draw_current_bsp bsp x_max y_max =
  let lines_l = Retrieve.lines_from_bsp bsp x_max y_max
  and rect_l = Retrieve.rectangles_from_bsp bsp x_max y_max
  and trace_r (rect, col) = 
  Graphics.set_color (Conversion.graphics_color_of_colour col Graphics.white);
  Graphics.fill_rect rect.xmin rect.ymin (rect.xmax - rect.xmin) (rect.ymax - rect.ymin)
  in
  List.iter trace_r rect_l;
  trace_lines lines_l
;;

let prepare_message () =
  Graphics.set_color white;
  Graphics.fill_rect 0 game_sizey window_sizex height_message_zone;
  Graphics.set_color black;
  Graphics.moveto 70 (game_sizey + 20);
  Graphics.set_text_size 20
;;

let display_win_message () = 
  prepare_message ();
  Graphics.draw_string "You won ! Congratulations ! "
;;
let display_continue_message () =
  prepare_message (); 
  Graphics.draw_string "Continue"
;;