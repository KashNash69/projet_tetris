(*open CPutil;;*)

(*#use CPutil.ml *)(*pour ouvrir Ocaml*)
(*graphique tetris
taille carre : 20
marge droite : 50
marge gauche : 50
marge bas : 50
marge haut : 100
taille bord de zone : 10
grille du tetris = 350 610
 *)

(* -------------------------- *)
(* -------------------------- *)
(*    fonctions utilitaires   *)
(* -------------------------- *)
(* -------------------------- *)

let mywait(x : float) : unit =
  let y : float ref = ref (Sys.time()) in
  while (Sys.time() -. !y) < x
  do ()
  done
;;

(* ------------------------------------------------- *)
(* ------------------------------------------------- *)
(*    Types, formes, parametrage et initialisation   *)
(* ------------------------------------------------- *)
(* ------------------------------------------------- *)


(* Types *)
(** 
descritption : point de l'environnement de travail
*)
type t_point = {
    x : int (** coordonnee en x *);
    y : int (** coordonnee en y *)
  } ;;

(** 
description de 'a t_array
*)
type 'a t_array = {
    len : int ;(** longueur de la liste*)
    value : 'a array (** tableau de type 'a*)
  } ;;

(** 
description de t_shape
*)
type t_shape = {
    shape : t_point list (** liste de point d'une certaine forme*);
    x_len : int ;(** amplitude en x *)
    y_len : int ;(** amplitude en y *)
    rot_rgt_base : t_point ;(** forme resultant d'une rotation droite *)
    rot_rgt_shape : int ; (** forme resultant d'une rotation droite *)
    rot_lft_base : t_point ;(** forme resultant d'une rotation gauche *)
    rot_lft_shape : int(** forme resultant d'une rotation gauche *)
  } ;; 

(** 
description de t_cur_shape
*)
type t_cur_shape = {
    base : t_point ref ;(** point de base de la forme dans l'espace de travail*)
    shape : int ref ;(** indice de la forme dans le tableau contenant le descriptif de la forme *)
    color : t_color ref (** couleur de la forme*)
  } ;;

(** 
description de t_param_time
*)
type t_param_time = {
    init : float ;(** duree initiale d'un deplacement*)
    extent : float ;(**duree entre 2 accelerations*)
    ratio : float(**coefficient d'acceleration*)
  } ;;

(** 
description de t_param_graphics
*)
type t_param_graphics ={
    base : t_point ;(** origine de l'espace d'affichage*)
    dilat : int ;(**taille d'un carre*)
    color_arr : t_color t_array (** tableau de couleur disponible*)
  } ;;

(** 
description de t_param
*)
type t_param ={
    time : t_param_time ; (** parametre de temps *)
    mat_szx : int ; mat_szy : int ;(**taille de la matrice de l'espace de travail*)
    graphics : t_param_graphics ; (**parametre graphique*)
    shapes : t_shape t_array(**tableau contenant les formes abstraites disponibles*)
  } ;;

(** 
description de t_play
*)
type t_play = {
    par : t_param ;(** parametre du jeu*)
    cur_shape : t_cur_shape ;(**descriptif de la forme actuelle *)
    mat : t_color matrix(** matrice decrivant l'espace de travail*)
  } ;;


(* Initialisation de quelques formes et des parametres *)

let init_sh011() : t_shape = 
  {shape = [{x = 0 ; y = 0} ; {x = 1 ; y = 0} ; {x = 2 ; y = 0} ; {x = 3 ; y = 0}] ; 
  x_len = 4 ; y_len = 1 ; 
  rot_rgt_base = {x = 1 ;  y = 1} ; rot_rgt_shape = 1 ; 
  rot_lft_base = {x = 2 ; y = 1} ; rot_lft_shape = 1} 
;;
let init_sh112() : t_shape = 
  {shape = [{x = 0 ; y = 0} ; {x = 0 ; y = -1} ; {x = 0 ; y = -2} ; {x = 0 ; y = -3}] ; 
  x_len = 1 ; y_len = 4 ; 
  rot_rgt_base = {x = -2 ;  y = -1} ; rot_rgt_shape = 0 ; 
  rot_lft_base = {x = -1 ; y = -1} ; rot_lft_shape = 0} 
;;
let init_sh211() : t_shape = 
  {shape = [{x = 0 ; y = 0} ; {x = 0 ; y = -1} ; {x = 1 ; y = 0} ; {x = 1 ; y = -1}] ; 
  x_len = 2 ; y_len = 2 ; 
  rot_rgt_base = {x = 0 ;  y = 0} ; rot_rgt_shape = 2 ; 
  rot_lft_base = {x = 0 ;  y = 0} ; rot_lft_shape = 2} 
;;

let init_shapes() : t_shape t_array = 
  {len = 3 ; value = [| init_sh011() ; init_sh112() ; init_sh211() |]} 
;;
let init_color() : t_color t_array = 
  {len = 7 ; value = [|blue ; red ; green ; yellow ; cyan ; magenta ; grey|]} ;;

let init_param() : t_param = 
    {
    time = {init = 1.0 ; extent = 10.0 ; ratio = 0.8} ; 
    mat_szx = 15 ; mat_szy = 28 ;
    graphics = {base = {x = 50 ; y = 50} ; dilat = 20 ; color_arr = init_color()} ; 
    shapes = init_shapes()
    }
;;

(* --------------------------------- *)
(* --------------------------------- *)
(*   Types et fonctions graphique    *)
(* --------------------------------- *)
(* --------------------------------- *)


let dilat : int = 20 ;;
(*notre base_draw prend (0,0) comme valeur donc on ne l'utilise pas dans la fonction convert*)

(**
description : convertie un point de l'esapce de travail en un point en pixel dans l'espace d'affichage 
@param p point dans la matrice de travail
@param base_draw point de base de la matrice graphique
@param dilat largeur de base d'un carre sur le graphique
@return le resultat de convert dans la matrice graphique
@author NICOLAS
 *)
let convert(p, base_draw, dilat : t_point * t_point * int) : t_point =
  {x = (p.x * dilat + 70); y = (p.y * dilat + 70)}
;;


(*QUESTION 1*)
(**
description : dessine un carre de taille dilat d'une couleur choisie en un point choisie
@param p point dans la matrice de travail
@param base_draw point de base de la matrice graphique
@param dilat largeur de base d'un carre sur le graphique
@param col couleur a utiliser
@author NICOLAS
 *)
let draw_absolute_pt(p, base_draw, dilat, col : t_point * t_point * int * t_color ) : unit =
  set_color(col);
  let new_p : t_point = convert(p, base_draw, dilat) in
  draw_rect(new_p.x, new_p.y, dilat - 1, dilat - 1)
;;

(**
description : dessine un carre plein de taille dilat d'une couleur choisie en un point choisie
@param p point dans la matrice de travail
@param base_draw point de base de la matrice graphique
@param dilat largeur de base d'un carre sur le graphique
@param col couleur a utiliser
@author NICOLAS
 *)
let fill_absolute_pt(p, base_draw, dilat, col : t_point * t_point * int * t_color ) : unit =
  set_color(col);
  let new_p : t_point = convert(p, base_draw, dilat) in
  fill_rect(new_p.x, new_p.y, dilat - 1, dilat - 1)
;;

(**
description : dessine un carre plein avec une bordure noir de taille dilat d'une couleur choisie en un point choisie
@param p point dans la matrice de travail
@param base_draw point de base de la matrice graphique
@param dilat largeur de base d'un carre sur le graphique
@param col couleur a utiliser
@author PIERRE
 *)
let drawfill_absolute_pt(p, base_draw, dilat, col : t_point * t_point * int * t_color) : unit =
  fill_absolute_pt(p, base_draw, dilat, col);
  draw_absolute_pt(p, base_draw, dilat, 0)
;;


(*QUESTION 2*)
(**
description : dessine un carre de taille dilat d'une couleur choisie 
@param p point dans la matrice de travail
@param base_point point du debut du dessin voulue
@param base_draw point de base de la matrice graphique
@param dilat largeur de base d'un carre sur le graphique
@param col couleur a utiliser
@author NICOLAS
 *)
let draw_relative_pt(p, base_point, base_draw, dilat, col : t_point * t_point * t_point * int * t_color) : unit =
  let new_p : t_point = {x = p.x + base_point.x ; y = p.y + base_point.y} in
  draw_absolute_pt(new_p, base_draw, dilat, col)
;;

(**
description : dessine un carre plein de taille dilat d'une couleur choisie 
@param p point dans la matrice de travail
@param base_point point du debut du dessin voulue
@param base_draw point de base de la matrice graphique
@param dilat largeur de base d'un carre sur le graphique
@param col couleur a utiliser
@author PIERRE
 *)
let fill_relative_pt(p, base_point, base_draw, dilat, col : t_point * t_point * t_point * int * t_color) : unit =
  let new_p : t_point = {x = p.x + base_point.x ; y = p.y + base_point.y} in
  fill_absolute_pt(new_p, base_draw, dilat, col)
;;

(**
description : dessine un carre plein avec une bordure noir de taille dilat d'une couleur choisie
@param p point dans la matrice de travail
@param base_point point du debut du dessin desire
@param base_draw point de base de la matrice graphique
@param dilat largeur de base d'un carre sur le graphique
@param col couleur a utiliser
@author PIERRE
 *)
let drawfill_relative_pt(p, base_point,  base_draw, dilat, col : t_point * t_point * t_point * int * t_color) : unit =
  fill_relative_pt(p, base_point, base_draw, dilat, col);
  draw_relative_pt(p, base_point, base_draw, dilat, 0)
;;


(*QUESTION 3*)

(**
description : dessine une suite de carre qui forment une shape, ils ont tous la meme couleur et taille
@param p_list liste de points dans la matrice de travail
@param base_point point du debut du dessin voulue
@param base_draw point de base de la matrice graphique
@param dilat largeur de base d'un carre sur le graphique
@param col couleur a utiliser
@author NICOLAS
 *)
let draw_pt_list(pt_list, base_point, base_draw, dilat, col : t_point list * t_point * t_point * int * t_color) : unit =
  (
    for i = 0 to len(pt_list) - 1 do
      draw_relative_pt(nth(pt_list, i), base_point, base_draw, dilat, col)
    done;
  )
;;

(**
description : dessine une suite de carre plein qui forment une shape, ils ont tous la meme couleur et taille
@param p_list liste de points dans la matrice de travail
@param base_point point du debut du dessin voulue
@param base_draw point de base de la matrice graphique
@param dilat largeur de base d'un carre sur le graphique
@param col couleur a utiliser
@author NICOLAS, PIERRE
 *)
let fill_pt_list(pt_list, base_point, base_draw, dilat, col : t_point list * t_point * t_point * int * t_color) : unit =
  (
    for i = 0 to len(pt_list) - 1 do
      fill_relative_pt(nth(pt_list, i), base_point, base_draw, dilat, col)
    done;
  )
;;

(**
description : dessine une suite de carre plein avec une bordure noir qui forment une shape, ils ont tous la meme couleur et taille
@param p_list liste de points dans la matrice de travail
@param base_point point du debut du dessin desire
@param base_draw point de base de la matrice graphique
@param dilat largeur de base d'un carre sur le graphique
@param col couleur a utiliser
@author PIERRE
 *)
let drawfill_pt_list(pt_list, base_point, base_draw, dilat, col : t_point list * t_point * t_point * int * t_color) : unit=
  fill_pt_list(pt_list,base_point,base_draw,dilat,col);
  draw_pt_list(pt_list,base_point,base_draw,dilat,0)
;;


(* QUESTION 4*)
(**
description :dessine le cadre de base de l'espace graphique du tetris
@param base_draw point de base de la matrice graphique
@param size_x largeur de l'espace de travail voulu
@param size_y hauteur de l'espace de travail voulu
@param dilat largeur de base d'un carre sur le graphique
@author LOUIS
 *)
let draw_frame(base_draw, size_x, size_y, dilat: t_point * int * int * int) : unit =
  let list_left : t_point list ref = ref [{x = 0; y = 0}]
  and list_right : t_point list ref = ref [{x = size_x; y = 0}]
  and list_down : t_point list ref = ref [] in
  (
    for i = 1 to (size_y) do
      (
        list_left := add_lst(!list_left, {x = 0; y = i});
        list_right := add_lst(!list_right, {x = size_x; y = i})
      )
    done;
    for i = 1 to (size_x) do
      list_down := add_lst(!list_down, {x = i; y = 0})
    done;
    let base_pt : t_point = {x = 0; y = 0}in
    (*and base_draw : t_point = {x = 0; y = 0} in*)
    (
      drawfill_pt_list(!list_left, base_pt, base_draw, dilat, black);
      drawfill_pt_list(!list_right, base_pt, base_draw, dilat, black);
      drawfill_pt_list(!list_down, base_pt, base_draw, dilat, black)
    )
  )
;;
(*auteur : Louis*)

(*Question 6*) (*auteur : Pierre*)

let getArrlen(prm : 'a t_array) : int =  prm.len;;
let getValue(prm : 'a t_array) : 'a array = prm.value;;
let getShape(prm : t_shape) : t_point list = prm.shape;;
let getShapeXlen(prm : t_shape) : int = prm.x_len;;
let getShapeYlen(prm : t_shape) : int = prm.y_len;;
let getCurBase(prm : t_cur_shape) : t_point ref = prm.base;;
let getCurShape(prm : t_cur_shape) : int ref = prm.shape;;
let getCurColor(prm : t_cur_shape) : t_color ref = prm.color;;
let getInitTime(prm : t_param_time) : float = prm.init;;
let getExtentTime(prm : t_param_time) : float = prm.extent;;
let getRatioTime(prm : t_param_time) : float = prm.ratio;;
let getGraphicBase(prm : t_param_graphics) : t_point = prm.base;;
let getGraphicDilat(prm : t_param_graphics) : int = prm.dilat;;
let getGraphicColor(prm : t_param_graphics) : t_color t_array = prm.color_arr;;
let getParamTime(prm : t_param) : t_param_time = prm.time;;
let getSizeX(prm : t_param) : int = prm.mat_szx;;
let getSiezY(prm : t_param) : int = prm.mat_szy;;
let getGraphics(prm : t_param) : t_param_graphics = prm.graphics;;
let getShapes(prm : t_param) : t_shape t_array = prm.shapes;;
let getParam(prm : t_play) : t_param = prm.par;;
let getCurShape(prm : t_play) : t_cur_shape = prm.cur_shape;;
let getMat(prm : t_play) : t_color matrix = prm.mat;;

(* QUESTION 7*)
(**
description : choisie une couleur aleatoirement dans une liste de couleur 
@param t tableau des couleurs possibles
@author PIERRE, MELIE
@return une couleur aleatoire du tableau t 
*)
let color_choice(t : t_color t_array) : t_color =
  let v : 'a array = getValue(t) and i : int = rand_int(0, getArrlen(t) - 1) in
  v.(i)
;;

(**
Description: le but est d'obtenir une forme aleatoire qui doit apparaitre a une position aleatoire dans l'espace de travail et d'une couleur aleatoire de façon a ce que la forme apparaisse le plus haut possible dans l'espace d'affichage et ne dois pas depasser les cotes.
@param shapes tableau contenant le nombre et les valeurs de chaque forme de type t_shape disponible
@param mat_szx largeur de l'espace de travail 
@param mat_szy hauteur de l'espace de travail
@param color_arr tableau des couleurs disponibles
@author PIERRE
 *)

let cur_shape_choice(shapes, mat_szx, mat_szy, color_arr : t_shape t_array * int * int * t_color t_array) : t_cur_shape =
  let select_shape : int ref = ref (rand_int(1, getArrlen(shapes))) in
  let shapes_value : 'a array = getValue(shapes) in
  let select_shape_value : t_shape = shapes_value.(!select_shape) in
  let base : t_point ref = ref{x = rand_int(0, mat_szx - getShapeXlen(select_shape_value));
                        y = mat_szy - getShapeYlen(select_shape_value)}; in
  let color : t_color ref = ref (color_choice(color_arr)) in
  {base = base; shape = select_shape; color = color}
;;

(* ----------------------------------------------- *)
(* ----------------------------------------------- *)
(*    Deplacements et controle des deplacements    *)
(* ----------------------------------------------- *)
(* ----------------------------------------------- *)


(*
(* choix des deplacements suivant le caractere saisi *)
let move(pl, dir : t_play * char) : bool = 
  (
  if dir = 't'
    then rotate_right(pl)
    else
      if dir = 'c'
      then rotate_left(pl)
      else
        if dir = 'd'
        then move_left(pl)
        else
          if dir = 'h'
          then move_right(pl)
          else () ;  
  (dir = 'v')
  )
;;


(* ----------------------------------- *)
(* ----------------------------------- *)
(*    Suppression des lignes pleines   *)
(* ----------------------------------- *)
(* ----------------------------------- *)


(* --------------------- *)
(* --------------------- *)
(*   Une etape de jeu    *)
(* --------------------- *)
(* --------------------- *)

let newstep(pl, new_t, t, dt : t_play * float ref * float * float) : bool = 
  let the_end : bool ref = ref (!new_t -. t > dt) and dec : bool ref = ref false in
  let dir : char ref = ref 'x' and notmove : bool ref = ref false in
    (
    while not(!the_end)
    do 
      if key_pressed()
      then dir := read_key()
      else () ;
      dec := move(pl, !dir) ;
      dir := 'x' ; 
      new_t := Sys.time() ;
      the_end := !dec || (!new_t -. t > dt) ;
    done ; 
    if !dec 
    then (move_at_bottom(pl) ; notmove := true)
    else notmove := not(move_down(pl)) ;
    if !notmove
    then the_end := final_newstep(pl)
    else the_end := false;
    !the_end ;
    )
;;

(* ------------------------ *)
(* ------------------------ *)
(*    Fonction principale   *)
(* ------------------------ *)
(* ------------------------ *)


let jeuCP2() : unit =
  let pl : t_play = init_play() in
  let t : float ref = ref (Sys.time()) and new_t : float ref = ref (Sys.time()) in
  let dt : float ref = ref (time_init(pl.par)) and t_acc : float ref = ref (Sys.time()) in
  let the_end : bool ref = ref false in
    while not(!the_end)
    do
      the_end := newstep(pl, new_t, !t, !dt) ; 
      if ((!new_t -. !t_acc) > time_extent(pl.par))
      then 
        (
        dt := !dt *. time_ratio(pl.par) ; 
        t_acc := !new_t
        ) 
      else () ;
      t := !new_t
    done
;;
 *)
