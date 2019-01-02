open Graphics

exception Quit;;

(* Initialise le jeu *)
let init_game () =
  let fbsp = Bsp.Generate.random_final_bsp 3 Ui.window_sizex Ui.game_sizey
  in
  let lines = Bsp.Retrieve.lines_from_bsp fbsp Ui.window_sizex Ui.game_sizey
  and bbsp = Bsp.Generate.initial_bsp_from_bsp fbsp
  in 
  Ui.trace_lines lines;
  (bbsp, fbsp, lines)
;;

(* Vérifie si la souris est dans la zone de jeu *)
let mouse_in_game_zone () = 
  let (x, y) = mouse_pos ()
  in x < Ui.window_sizex && y < Ui.game_sizey
;;


let rec loop current_color bsp fbsp lines : unit = 
  let event = wait_next_event [Button_down; Key_pressed] 
  in 
  (* S'il y a eu un clic de souris *)
  let invalid_click = not (mouse_in_game_zone()) || (Bsp.is_in_line lines event.mouse_x event.mouse_y)
  in
  if event.button && not (invalid_click) then
    let bsp' = Bsp.colorise_clicked_rectangle bsp Ui.window_sizex Ui.game_sizey current_color event.mouse_x event.mouse_y
    in 
    let message = if Bsp.check_current bsp' Ui.window_sizex Ui.game_sizey lines
    then Ui.display_win_message ()
    else Ui.display_continue_message ()
    and next_c = Ui.next_color current_color
    in
    Ui.draw_current_bsp bsp' Ui.window_sizex Ui.game_sizey;
    Ui.trace_lines lines;
    message;
    set_color next_c;    
    loop next_c bsp' fbsp lines;
  else if event.keypressed && event.key = 'S' then
    begin
      Ui.draw_current_bsp fbsp Ui.window_sizex Ui.game_sizey;
      Ui.trace_lines lines;
    end
  else loop current_color bsp fbsp lines;
in
Ui.init_window();
Ui.display_information();
let (bsp, fbsp, lines) = init_game ()
in
set_color red;
try loop red bsp fbsp lines
with 
| Quit -> close_graph ()
| Graphic_failure ("fatal I/O error") -> close_graph()