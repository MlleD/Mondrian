open Graphics

exception Quit;;

let init () : unit = 
  open_graph " 600x400";
  set_window_title "Mondrian";
  let fbsp = Mondrian.random_final_bsp 3 (Graphics.size_x()) (Graphics.size_y())
  in Mondrian.draw_current_bsp fbsp (Graphics.size_x()) (Graphics.size_y())
;;

let rec loop () : unit = 
  let event = wait_next_event [Button_down; Key_pressed] 
  in 
  loop()
in
init();
try loop ()
with Quit -> close_graph ()