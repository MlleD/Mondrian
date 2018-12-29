(* Couleurs *)
type colour = Red | Blue | Magenta

(* BSP *)
type label =
    {
      coord : int;
      colored : bool;
    }

type bsp = R of colour option | L of label * bsp * bsp;;

(* Autres types *)
type rectangle =
    {
      p1 : int * int;
      p2 : int * int;
      p3 : int * int;
      p4 : int * int;
    }

type line =
    {
      p1 : int * int;
      p2 : int * int;
    }

(* Exemple de bsp du sujet du sujet *)
let bsp_figure2 =
  L({coord = 545; colored = false},
    L({coord = 760; colored = false},
      L({coord = 200; colored = false},
	R None,
	R None
      ),
      L({coord = 125; colored = false},
	R None,
	R None
      )
    ),
    L({coord = 345; colored = false},
      L({coord = 750; colored = false},
	R None,
	R None
      ),
      L({coord = 600; colored = false},
	R None,
	L({coord = 500; colored = false},
	  R None,
	  R None
	)
      )
    )
  )

(* Fonction renvoyant la liste des rectangles d’un BSP. *)
let rectangles_from_bsp bsp x_min x_max y_min y_max = 
  let rec aux bsp x_min x_max y_min y_max par r_list =
    match bsp with
	R(_) -> [{p1 = (x_min, y_min); p2 = (x_max, y_min); p3 = (x_min, y_max); p4 = (x_max, y_max)}]
      | L(l, g, d) ->
	if par mod 2 = 0 then
	  (aux d l.coord x_max y_min y_max (par + 1) r_list) @
	  (aux g x_min l.coord y_min y_max (par + 1) r_list)
	else
	  (aux d x_min x_max l.coord y_max (par + 1) r_list) @
	  (aux g x_min x_max y_min l.coord (par + 1) r_list)
  in aux bsp x_min x_max y_min y_max 0 [] 

(* Fonction renvoyant la liste des lignes de séparation d’un BSP avec leurs couleurs. *)
let lines_from_bsp bsp x_min x_max y_min y_max =
  let aux (rect : rectangle) = [{p1 = rect.p1; p2 = rect.p2}; {p1 = rect.p2; p2 = rect.p3};
				{p1 = rect.p3; p2 = rect.p4}; {p1 = rect.p4; p2 = rect.p1}]
  in
  List.flatten (List.map aux (rectangles_from_bsp bsp x_min x_max y_min y_max))

(* lines_from_bsp bsp_figure2 0 1000 0 1000;; *)

(* Fonction générant un nombre aléatoire dans un intervalle d'entiers [min; max[ *)
let random min max = 
  Random.self_init();
  min + (Random.int (max - min))

(* Fonction générant une couleur aléatoire *)
let random_colour () =
  let c = Random.bool() in
  if c then Some Red
  else Some Blue

(* Fonctions de conversion *)
(* Conversion de colour vers string *)
let string_of_colour c = 
  if c = Some Red then "Red"
  else if c = Some Blue then "Blue"
  else if c = Some Magenta then "Magenta"
  else "None"

(* Conversion de colour vers Graphics.color *)
(* Paramètres : c, couleur à convertir et none, la valeur à renvoyer si c vaut None*)
let graphics_color_of_colour c none =
  if c = Some Red then Graphics.red
  else if c = Some Blue then Graphics.blue
  else if c = Some Magenta then Graphics.magenta
  else none

(* Conversion de Graphics.color vers colour *)
(* Paramètres : c, couleur à convertir et none, la valeur à renvoyer si c vaut None*)
let colour_of_graphics_color c none =
  if c = Graphics.red then Some Red
  else if c = Graphics.blue then Some Blue
  else if c = Graphics.magenta then Some Magenta
  else none

(* Conversion de label vers string *)
let string_of_label l =
  "{coord = " ^ (string_of_int l.coord) ^ "; colored = " ^ (string_of_bool l.colored) ^ "}"

(* Conversion de bsp vers string *)
let rec string_of_bsp bsp =
  match bsp with
  R c -> "R " ^ (string_of_colour c)
  | L (l, g, d) -> "L (" ^ (string_of_label l) ^ "), " ^ (string_of_bsp g) ^ (string_of_bsp d)

(* Fonction générant une configuration finale aléatoire du jeu *)
(* Parametres : depth_max (profondeur du bsp), x_max (abscisse maximale), y_max (ordonnée maximale) *)
let random_final_bsp depth_max x_max y_max = 
  let rec add_node current_depth x_min x_max y_min y_max = 
    if (current_depth = depth_max) || (x_min = x_max) || (y_min = y_max) then
      R (random_colour ())
    else
      let next_depth = current_depth + 1 in
      if (current_depth mod 2) = 0 then
        let random_x = random x_min x_max
        in L ( {coord = random_x; colored = Random.bool() },
                add_node next_depth x_min (random_x - 1) y_min y_max,
                add_node next_depth (random_x + 1) x_max y_min y_max
        )
      else
        let random_y = random y_min y_max
        in L ( {coord = random_y; colored = Random.bool() },
                add_node next_depth x_min x_max (random_y - 1) y_max,
                add_node next_depth x_min x_max y_min (random_y + 1)
        )
  in add_node 0 1 x_max 1 y_max

type rectangle2 = {xmin: int ; xmax : int ; ymin : int ; ymax : int}
;;
let rectangles_from_bsp bsp x_max y_max = 
  let rec list_rectangles parity_depth bsp x_min x_max y_min y_max acc =
    match bsp with
    | R c -> ({xmin = x_min; xmax = x_max; ymin = y_min; ymax = y_max}, c)::[]
    | L (l, left, right) -> 
      if parity_depth then
        let rectangle_left = list_rectangles false left x_min l.coord y_min y_max acc
        and rectangle_right = list_rectangles false right l.coord x_max y_min y_max acc in
        rectangle_left@rectangle_right@acc
      else
        let rectangle_low = list_rectangles true left x_min x_max y_min l.coord acc 
        and rectangle_high = list_rectangles true right x_min x_max l.coord y_max acc in
        rectangle_low@rectangle_high@acc
  in list_rectangles true bsp 1 x_max 1 y_max []
;;

(* Determine la couleur du noeud racine du bsp passé en parametre *)
let rec root_line_color bsp x_max y_max= 
  match bsp with
  | R c -> c
  | L (l, left, right) -> 
    if not l.colored then
      None
    else
      match (left, right) with
      | L (a, b, c) , R d | R d, L (a, b, c) ->
        (* Cas où un fils est un label et l'autre un rectangle *)
        let color_R = root_line_color (R d) x_max y_max 
        and rectangles = rectangles_from_bsp (L (a, b, c)) x_max y_max in
        let filtered = List.filter (function (r, c) -> List.mem a.coord [r.xmin; r.xmax; r.ymin; r.ymax]) rectangles
        in 
        let (nbR, nbB) =
          let rec count_colors list nbR nbB = 
          match list with
          | [] -> (nbR, nbB)
          | h::t -> 
            match (snd h) with
            | Some Red -> count_colors t (nbR + 1) nbB
            | _ -> count_colors t nbR (nbB + 1)
          in count_colors filtered 0 0
        in 
        if (nbB = nbR) || (nbR = (nbB + 1) && color_R = Some Blue) || (nbB = (nbR + 1) && color_R = Some Red) then Some Magenta
        else if nbR > nbB then Some Red
        else Some Blue
      | _ -> (* Cas où les deux fils sont tous les deux des Labels ou des Rectangles *)
        let color_left = root_line_color left x_max y_max and color_right = root_line_color right x_max y_max in
        if color_left = color_right then color_left
        else if (color_left = Some Magenta) then color_right
        else if (color_right = Some Magenta) then color_left
        else Some Magenta

(*parcours prefixe : racine puis fils gauche puis fils droit*)
let lines_from_bsp bsp x_max y_max = 
  let rec list parity_depth bsp x_min x_max y_min y_max acc =
    match bsp with
    | R c -> acc
    | L (l, left, right) -> 
        if parity_depth then
        ({xmin = l.coord; xmax = l.coord; ymin = y_min; ymax = y_max}, root_line_color bsp x_max y_max)::(list false left x_min l.coord y_min y_max acc)@(list false right l.coord x_max y_min y_max acc)
        else
        ({xmin = x_min; xmax = x_max; ymin = l.coord; ymax = l.coord}, root_line_color bsp x_max y_max)::(list true left x_min x_max y_min l.coord acc)@(list true right x_min x_max l.coord y_max acc)
  in list true bsp 1 x_max 1 y_max []
;;

(* Trace les lignes (avec leur couleur) de la liste de lignes passée en paramètre *)
let trace_lines lines_list = 
  let aux (rect, col) = 
	Graphics.moveto rect.xmin rect.ymin;
	Graphics.set_color (graphics_color_of_colour col Graphics.black);
  Graphics.lineto rect.xmax rect.ymax
  in List.iter aux lines_list
;;
  
(* Fonction affichant, sur le canevas graphique, la configuration courante du joueur. *)
let draw_current_bsp bsp x_max y_max =
  let lines_l = lines_from_bsp bsp x_max y_max
  and rect_l = rectangles_from_bsp bsp x_max y_max
  and trace_r (rect, col) = 
  Graphics.set_color (graphics_color_of_colour col Graphics.white);
  Graphics.fill_rect rect.xmin rect.ymin (rect.xmax - rect.xmin) (rect.ymax - rect.ymin)
  in
  List.iter trace_r rect_l;
  trace_lines lines_l
;;

(* Renvoie le bsp initial correspondat au bsp passé en passé en paramètre *)
let rec initial_bsp_from_bsp bsp = 
    match bsp with
    | R color -> R None
    | L (l, left, right) -> L (l, initial_bsp_from_bsp left, initial_bsp_from_bsp right)
;;

(* Renvoie le bsp modifié avec le rectangle colorié *)
let colorise_clicked_rectangle bsp x_max y_max current_color mouse_x mouse_y = 
  let rec parcours bsp' parity_depth x_min x_max' y_min y_max' = 
    match bsp' with
    | R c -> 
    (* Si la souris est dans ce rectangle *)
    if (mouse_x >= x_min) && (mouse_x <= x_max') && (mouse_y >= y_min) && (mouse_y <= y_max')
    then R (colour_of_graphics_color current_color None)
    else R c
    | L (l, left, right) ->
    if parity_depth then L (l, parcours left false x_min l.coord y_min y_max', parcours right false l.coord x_max' y_min y_max')
    else L(l, parcours left true x_min x_max' y_min l.coord , parcours right true x_min x_max' l.coord y_max')
  in parcours bsp true 1 x_max 1 y_max
;;

(* Vérifie si la souris est sur une ligne de la liste de lignes line_list *)
let is_in_line line_list mouse_x mouse_y =
  List.exists (fun (line, col) -> mouse_x = line.xmin || mouse_x = line.xmax || mouse_y = line.ymin || mouse_y = line.ymax) line_list
;;