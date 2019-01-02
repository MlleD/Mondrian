open Graphics

exception Quit;;

(* Initialise le jeu *)
let init_game () =
  let fbsp = Bsp.Generate.random_final_bsp 3 (Graphics.size_x()) (Graphics.size_y())
  in
  let lines = Bsp.Retrieve.lines_from_bsp fbsp (Graphics.size_x()) (Graphics.size_y())
  and bbsp = Bsp.Generate.initial_bsp_from_bsp fbsp
  in 
  Ui.trace_lines lines;
  (bbsp, lines)
;;

let rec loop current_color bsp lines : unit = 
  let event = wait_next_event [Button_down; Key_pressed] 
  in 
  (* S'il y a eu un clic de souris *)
  if event.button then 
  let bsp' = if Bsp.is_in_line lines event.mouse_x event.mouse_y then bsp else
  Bsp.colorise_clicked_rectangle bsp (Graphics.size_x()) (Graphics.size_y()) current_color event.mouse_x event.mouse_y
  and next_c = Ui.next_color current_color
  in
  Ui.draw_current_bsp bsp' (Graphics.size_x()) (Graphics.size_y());
  Ui.trace_lines lines;
  set_color next_c;
  loop next_c bsp' lines;
  else loop current_color bsp lines
in
Ui.init_window();
let (bsp, lines) = init_game ()
in
set_color red;
try loop red bsp lines
with 
| Quit -> close_graph ()
| Graphic_failure ("fatal I/O error") -> close_graph()