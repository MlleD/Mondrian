open Graphics
open Bsp

(* Initialise la fenêtre graphique *)
let init_window () : unit = 
  open_graph " 600x400";
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