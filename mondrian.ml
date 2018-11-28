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
  let rec auxD bsp x_min x_max y_min y_max l =
        match bsp with
	    R (color) -> {p1 = (x_min, y_min); p2 = (x_max, y_min); p3 = (x_max, y_max); p4 = (x_min, y_max)} :: l
	  | L(l, g, d) -> auxG bsp d l.coord x_max y_min y_max
  and auxG bsp x_min x_max y_min y_max l =
        match bsp with
	R (color) ->
	  {p1 = (x_min, y_min); p2 = (x_max, y_min); p3 = (x_max, y_max); p4 = (x_min, y_max)} :: l
      | L(l, g, d) ->
	if par mod 2 = 0
	then
	  let _ = (aux bsp d l.coord x_max y_min y_max (par + 1))
	  in
	  (aux bsp g x_min l.coord y_min y_max (par + 1))
	else
	  let _ = (aux bsp d x_min x_max l.coord y_max (par + 1))
	  in
	  (aux bsp g x_min x_max y_min l.coord (par + 1))
in auxD bsp x_min x_max y_min y_max []
;;

(* Fonction renvoyant la liste des lignes de séparation d’un BSP avec leurs couleurs. *)
let lines_from_bsp bsp x_min x_max y_min y_max = ""
;;


