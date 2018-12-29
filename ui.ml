open Graphics

exception Quit;;

(* Initialise la fenêtre graphique *)
let init () : unit = 
  open_graph " 600x400";
  set_window_title "Mondrian";
  ;;

(* Initialise le jeu *)
let init_game () =
  let fbsp = Mondrian.random_final_bsp 3 (Graphics.size_x()) (Graphics.size_y())
  in
  let lines = Mondrian.lines_from_bsp fbsp (Graphics.size_x()) (Graphics.size_y())
  and bbsp = Mondrian.initial_bsp_from_bsp fbsp
  in 
  Mondrian.trace_lines lines;
  (bbsp, lines)
;;


(* Renvoie la couleur suivante *)
let next_color current_color = 
  if current_color = red then blue
  else if current_color = blue then white
  else red
;;

let rec loop current_color bsp lines : unit = 
  let event = wait_next_event [Button_down; Key_pressed] 
  in 
  (* S'il y a eu un clic de souris *)
  if event.button then 
  let bsp' = if Mondrian.is_in_line lines event.mouse_x event.mouse_y then bsp else
  Mondrian.colorise_clicked_rectangle bsp (Graphics.size_x()) (Graphics.size_y()) current_color event.mouse_x event.mouse_y
  and next_c = next_color current_color
  in
  Mondrian.draw_current_bsp bsp' (Graphics.size_x()) (Graphics.size_y());
  Mondrian.trace_lines lines;
  set_color next_c;
  loop next_c bsp' lines;
  else loop current_color bsp lines
in
init();
let (bsp, lines) = init_game ()
in
set_color red;
try loop red bsp lines
with Quit -> close_graph ()