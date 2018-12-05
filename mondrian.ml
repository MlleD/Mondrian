
open Graphics;;

(* Types donnés dans le sujet. *)
type label =
    {
      coord : int;
      colored : bool;
    }
;;

type bsp = R of color option | L of label * bsp * bsp;;

(* Autres types *)
type rectangle =
    {
      p1 : int * int;
      p2 : int * int;
      p3 : int * int;
      p4 : int * int;
    }
;;

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
;;

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
;;

(* Fonction renvoyant la liste des lignes de séparation d’un BSP avec leurs couleurs. *)
let lines_from_bsp bsp x_min x_max y_min y_max =
  let aux (rect : rectangle) = [{p1 = rect.p1; p2 = rect.p2}; {p1 = rect.p2; p2 = rect.p3};
				{p1 = rect.p3; p2 = rect.p4}; {p1 = rect.p4; p2 = rect.p1}]
  in
  List.flatten (List.map aux (rectangles_from_bsp bsp x_min x_max y_min y_max))
;;

lines_from_bsp bsp_figure2 0 1000 0 1000;;


