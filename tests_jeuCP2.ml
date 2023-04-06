(*
open CPutil;;
open JeuCP2;;
 *)

(* ---------------------------- *)
(* test de : convert            *)
(* ---------------------------- *)
(**
fonction de test fonctionnel de la fonction convert qui verifie la conversion 
@param status recupere l'environnement de test
@author LOUIS
 *)
(* *)
let test_convert_fonctional_1(status: t_test_status): unit =
  let test_step : t_test_step = test_start(status, "convert_fonctional_1")
  and p : t_point = {x = 2; y = 2} 
  and p_sorti : t_point = {x = 110; y = 110} in
  let test_result : t_point t_test_result = test_exec(test_step, convert, (p, {x = 2; y = 2}, dilat)) in
  (
    if test_is_success(test_result)
    then assert_equals(test_step, "carre(2;2)", test_get(test_result), p_sorti)
    else test_error(test_step);
    test_end(test_step)
  )
;;


(* ---------------------------- *)
(* test de : draw_absolute_pt   *)
(* ---------------------------- *)
(**
fonction de test fonctionnel de la fonction graphique draw_absolute_pt qui verifie en demandant a l'utilisateur s'il voit un carre noir 
@param status recupere l'environnement de test
@author MELIE, LOUIS 
 *)
let test_draw_absolute_pt_functional_1(status: t_test_status) : unit =
  let test_step : t_test_step = test_start(status, "draw_absolute_pt_functional_1")
  and p : t_point = {x = 2; y = 2} in
  let test_result : unit t_test_result = test_exec(test_step, draw_absolute_pt, (p, {x = 0; y = 0}, dilat, black)) in
  (
    if test_is_success(test_result)
    then
      (
        set_color(black);
        fill_rect(50, 50, 10, 450);
        fill_rect(260, 50, 10, 450);
        fill_rect(50, 50, 220, 10);
        draw_absolute_pt(p, {x = 0; y = 0}, dilat, black);
        print_string("Voyez vous un carre noir vide en (2;2) (oui/non)");
        let reponse : string = read_line() in
        assert_equals(test_step, "carre (2;2)", reponse, "oui")
      )
    else test_error(test_step);
    test_end(test_step);
    clear_graph();
    set_color(black);

  )
;;



(* ---------------------------- *)
(* test de : fill_absolute_pt   *)
(* ---------------------------- *)
(**
fonction de test fonctionnel de la fonction graphique fill_absolute_pt qui verifie en demandant a l'utilisateur s'il voit un carre noir plein 
@param status recupere l'environnement de test
@author MELIE, LOUIS
 *)
let test_fill_absolute_pt_functional_1(status: t_test_status) : unit =
  let test_step : t_test_step = test_start(status, "fill_absolute_pt_functional_1")
  and p : t_point = {x = 2; y = 2} in
  let test_result : unit t_test_result = test_exec(test_step, fill_absolute_pt, (p, {x = 0; y = 0}, dilat, black )) in
  (
    if test_is_success(test_result)
    then
      (
        set_color(black);
        clear_graph();
        fill_rect(50, 50, 10, 450);
        fill_rect(260, 50, 10, 450);
        fill_rect(50, 50, 220, 10);
        fill_absolute_pt(p, {x = 0; y = 0}, dilat, black);
        print_string("Voyez vous un carre noir plein en (2;2) (oui/non)");
        let reponse_2 : string = read_line() in
        assert_equals(test_step, "carre (2;2)", reponse_2, "oui")
      )
    else test_error(test_step);
    test_end(test_step);
    clear_graph();
    set_color(black);
  )
;;

(* ---------------------------- *)
(* test de :drawfill_absolute_pt*)
(* ---------------------------- *)
(**
fonction de test fonctionnel de la fonction graphique drawfill_absolute_pt qui verifie en demandant a l'utilisateur s'il voit carre bleu plein avec une bordure noire
@param status recupere l'environnement de test
@author NICOLAS, MELIE
 *)
let test_drawfill_absolute_pt_functional_1(status: t_test_status) : unit =
  let test_step : t_test_step = test_start(status, "drawfill_absolute_pt_functional_1")
  and p : t_point = {x = 2; y = 2} in
  let test_result : unit t_test_result = test_exec(test_step, drawfill_absolute_pt, (p, {x = 0; y = 0}, dilat,blue)) in
  (
    if test_is_success(test_result)
    then
      (
        set_color(black);
        fill_rect(50, 50, 10, 450);
        fill_rect(260, 50, 10, 450);
        fill_rect(50, 50, 220, 10);
        drawfill_absolute_pt(p, {x = 0; y = 0}, dilat, blue);
        print_string("Voyez vous un carre bleu avec une bordure noir en (2;2) (oui/non)");
        let reponse_3 : string = read_line() in
        assert_equals(test_step, "carre (2;2)", reponse_3, "oui")
      )
    else test_error(test_step);
    test_end(test_step);
    clear_graph();
    set_color(black);
  )
;;

(* ------------------------------- *)
(*     test draw_relative_pt       *)
(* ------------------------------- *)
(**
fonction de test fonctionnel de la fonction graphique draw_relative_pt qui verifie en demandant a l'utilisateur s'il voit un carre bleu vide au coordonees de la base base_point et en fonction du vecteur p
@param status recupere l'environnement de test
@author NICOLAS, MELIE
 *)
let test_draw_relative_pt_functional_1(status: t_test_status) : unit =
  let test_step : t_test_step = test_start(status, "draw_relative_pt_functional_1")
  and p : t_point = {x = 1; y = 2} in
  let test_result : unit t_test_result = test_exec(test_step, draw_relative_pt, (p,{x = 0; y = 0}, {x = 0; y = 0}, dilat, blue)) in
  (
    if test_is_success(test_result)
    then
      (
        set_color(black);
        fill_rect(50, 50, 10, 450);
        fill_rect(260, 50, 10, 450);
        fill_rect(50, 50, 220, 10);
        draw_relative_pt(p,{x = 0; y = 0}, {x = 0; y = 0}, dilat, blue);
        print_string("Voyez vous un carre bleu vide en (1;2) (oui/non)");
        let reponse_4 : string = read_line() in
        assert_equals(test_step, "carre (1;2)", reponse_4, "oui")
      )
    else test_error(test_step);
    test_end(test_step);
    clear_graph();
    set_color(black);
  )
;;

(* ---------------------------*)
(* test de : fill_relative_pt *)
(* ---------------------------*)
(**
fonction de test fonctionnel de la fonction graphique fill_relative_pt qui verifie en demandant a l'utilisateur s'il voit un carre noir plein au coordonees de la base base_point et en fonction du vecteur p
@param status recupere l'environnement de test
@author MELIE
 *)
let test_fill_relative_pt_functional_1(status: t_test_status) : unit =
  let test_step : t_test_step = test_start(status, "fill_relative_pt_functional_1")
  and p : t_point = {x = 2; y = 2} in
  let test_result : unit t_test_result = test_exec(test_step, fill_relative_pt, (p,{x = 2; y = 2}, {x = 0; y = 0}, dilat, black)) in
  (
    if test_is_success(test_result)
    then
      (
        set_color(black);
        fill_rect(50, 50, 10, 450);
        fill_rect(260, 50, 10, 450);
        fill_rect(50, 50, 220, 10);
        fill_relative_pt(p,{x = 2; y = 2}, {x = 0; y = 0}, dilat, black);
        print_string("Voyez vous un carre noir plein en (4;4) (oui/non)");
        let reponse_5 : string = read_line() in
        assert_equals(test_step, "carre noir plein en (4;4)", reponse_5, "oui")
      )
    else test_error(test_step);
    test_end(test_step);
     clear_graph();
    set_color(black);
  )
;;

(* ---------------------------- *)
(*  test drawfill_relative_pt   *)
(* ---------------------------- *)
(**
fonction test fonctionnel de la fonction graphique drawfill_relative_pt qui verifie en demandant a l'utilisateur s'il voit un carre bleu plein avec des bordures noires au coordonees de la base base_point et en fonction du vecteur p
@param status recupere l'environnement de test
@author NICOLAS
 *)
let test_drawfill_relative_pt_functional_1(status : t_test_status) : unit =
  let test_step : t_test_step = test_start(status, "drawfill_relative_pt_functional_1")
  and p : t_point = {x = 1; y = 1} in
  let test_result : t_point t_test_result = test_exec(test_step, convert, (p, {x = 0; y = 0}, dilat)) in
  (
    if test_is_success(test_result)
    then
      (
        set_color(black);
        open_graph(350, 610);
        fill_rect(50, 50, 10, 450);
        fill_rect(260, 50, 10, 450);
        fill_rect(50, 50, 220, 10);
        drawfill_relative_pt(p,{x = 1; y = 1}, {x = 0; y = 0}, dilat, blue);
        print_string("Voyez vous un carre bleu avec une bordure noir en (2;2) (oui/non)");
        let reponse_6 : string = read_line() in
        assert_equals(test_step, "carre bleu avec bordure noir (2;2)", reponse_6, "oui")
      )
    else test_error(test_step);
    test_end(test_step);
    clear_graph();
    set_color(black);
  )
;;

(* ---------------------------- *)
(*     test draw_pt_list        *)
(* ---------------------------- *)
(**
fonction test fonctionel de la fonction graphique draw_pt_list qui verifie en demandant a l'utilisateur s'il voit 3 carres noir vide a la suite 
@param status recupere l'environnement de test
@author NICOLAS
 *)
let test_draw_pt_list_functional_1(status : t_test_status) : unit =
  let test_step : t_test_step = test_start(status, "draw_pt_list_functional_1")
  and pt_list : t_point list = [{x = 0; y = 0};{x = 0; y = 1};{x = 0; y = 2}] in
  let test_result : unit t_test_result = test_exec(test_step, draw_pt_list, (pt_list,{x = 1; y = 1}, {x = 0; y = 0}, dilat, black)) in
  (
    if test_is_success(test_result)
    then
      (
        set_color(black);
        fill_rect(50, 50, 10, 450);
        fill_rect(260, 50, 10, 450);
        fill_rect(50, 50, 220, 10);
        draw_pt_list(pt_list,{x = 1; y = 1}, {x = 0; y = 0}, dilat, black);
        print_string("Voyez vous trois carres noir vide en ligne de (1;1) en (1;4) (oui/non)");
        let reponse_7 : string = read_line() in
        assert_equals(test_step, "ligne verticale de carre de (1;1) en (1;4)", reponse_7, "oui")
      )
    else test_error(test_step);
    test_end(test_step);
    clear_graph();
    set_color(black);
  )
;;

(* -----------------------*)
(* test de : fill_pt_list *)
(* -----------------------*)
(**
fonction test fonction de la fonction graphique fill_pt_list qui verifie en demandant a l'utilisateur s'il voit 3 carres rouge a la suite en donnant une forme
@param status recupere l'environnement de test
@author MELIE
 *)
let test_fill_pt_list_functional_1(status: t_test_status) : unit =
  let test_step : t_test_step = test_start(status, "fill_pt_list_functional_1")
  and pt_list : t_point list = [{x = 2; y = 2}; {x = 1; y = 1}; {x = 1; y = 2}] in
  let test_result : unit t_test_result = test_exec(test_step, fill_pt_list, (pt_list,{x = 2; y = 2}, {x = 0; y = 0}, dilat, red)) in
  (
    if test_is_success(test_result)
    then
      (
        set_color(black);
        fill_rect(50, 50, 10, 450);
        fill_rect(260, 50, 10, 450);
        fill_rect(50, 50, 220, 10);
        fill_pt_list(pt_list,{x = 2; y = 2}, {x = 0; y = 0}, dilat, red);
        print_string("Voyez vous 3 carres rouge plein) (oui/non)");
        let reponse_8 : string = read_line() in
        assert_equals(test_step, "3 carres rouge plein", reponse_8, "oui")
      )
    else test_error(test_step);
    test_end(test_step);
    clear_graph();
    set_color(black);
  )
;;

(* ---------------------------*)
(* test de : drawfill_pt_list *)
(* ---------------------------*)
(**
fonction test fonctionel de la fonction graphique drawfill_pt_list qui verifie en demandant a l'utilisateur s'il voit 3 carres rouge avec des bordures noire
@param status recupere l'environnement de test
@author NICOLAS
 *)
let test_drawfill_pt_list_functional_1(status: t_test_status) : unit =
  let test_step : t_test_step = test_start(status, "drawfill_pt_list_functional_1")
  and pt_list : t_point list = [{x = 2; y = 3}; {x = 3; y = 3}; {x = 3; y = 4}] in
  let test_result : unit t_test_result = test_exec(test_step, drawfill_pt_list, (pt_list,{x = 2; y = 2}, {x = 0; y = 0}, dilat, red)) in
  (
    if test_is_success(test_result)
    then
      (
        set_color(black);
        fill_rect(50, 50, 10, 450);
        fill_rect(260, 50, 10, 450);
        fill_rect(50, 50, 220, 10);
        drawfill_pt_list(pt_list,{x = 2; y = 2}, {x = 0; y = 0}, dilat, red);
        print_string("Voyez vous 3 carres rouge plein avec une bordure noire) (oui/non)");
        let reponse_9 : string = read_line() in
        assert_equals(test_step, "3 carres rouge plein avec bordure noire", reponse_9, "oui")
      )
    else test_error(test_step);
    test_end(test_step);
    clear_graph();
    set_color(black);
  )
;;


(* ---------------------*)
(* test de : draw_frame *)
(* ---------------------*)
(**
fonction de test fonctionnel de la  fonction graphique draw_frame qui affiche un cadre
@param status recupere l'environnement de test
@author MELIE

 *)
let test_draw_frame_functional_1(status: t_test_status) : unit =
  let test_step : t_test_step = test_start(status, "test_draw_frame_functional_1") in

  let test_result : unit t_test_result = test_exec(test_step, draw_frame, ({x = 0; y = 0},5,10, dilat)) in
  (
    if test_is_success(test_result)
    then
      (
        set_color(black);
        draw_frame({x = 0; y = 0},5,10, dilat);        
        print_string("Voyez vous cadre  qui delimite une zone d'affichage de (5,10) (oui/non)");
        let reponse_10 : string = read_line() in
        assert_equals(test_step, "cadre delimite une zone d'affichage de (5,10)", reponse_10, "oui")
      )
    else test_error(test_step);
    test_end(test_step);
    clear_graph();
    set_color(black);
  )
;;


(* ----------------------*)
(*  test de : getArrLen  *)
(* ----------------------*)
(**
fonction test fonctionel de la fonction getArrLen qui verifie que la fonction produit bien la bonne longueur du tableau
@param status recupere l'environnement de test
@author LOUIS
 *)
let test_getArrLen_functionnal(status : t_test_status) : unit =
  let test_step : t_test_step = test_start(status, "getArrlen functionnal")
  and prm_1 : int t_array = {len = 12; value = arr_make(12, 0)}in
  (
    let test_result : int t_test_result = test_exec(test_step, getArrlen, prm_1) in
    (
      if test_is_success(test_result)
      then
        (
          assert_equals(test_step, "la longueur est bien 12", test_get(test_result), 12)
        )
      else test_error(test_step);
      test_end(test_step)
    )
  )
;;


(* -------------------------*)
(*  test de : color_choice  *)
(* -------------------------*)
(**
fonction test structural de la fonction color_choice qui verifie que la fonction renvoi une couleur qui est comprise dans t_color
@param status recupere l'environnement de test
@author LOUIS, MELIE
 *)
let test_color_choice_structural(status : t_test_status) : unit =
  let test_step : t_test_step = test_start(status, "color_choice structural")
  and all_colors : t_color t_array = {len = 9; value=[| black ; white ; blue ; red ; green ; yellow ; cyan ; magenta ; grey |]}
  and is_t_color(colors, result: t_color t_array * 'a): bool =
    let colors_list : t_color list = list_of_array(colors.value) in
    list_contains_value(colors_list, result)
  in
  (
    let test_result : t_color t_test_result = test_exec(test_step, color_choice, all_colors) in
    (
      if test_is_success(test_result)
      then
        (
          assert_true(test_step, "la couleur est bien comprise dans le tableau",is_t_color(all_colors, test_get(test_result)))
        )
      else test_error(test_step);
      test_end(test_step)
    )
  )
;;


(* -----------------------------*)
(*  test de : cur_shape_choice  *)
(* -----------------------------*)

(**
fonction de test structural de la fonction cur_shape_choice qui verifie qu'on obtient bien une shape aleatoire dans le tableau t_shape t_array
@param status recupere l'environnement de test
@author LOUIS, MELIE
 *)
let test_cur_shape_choice_structural(status:t_test_status) : unit =
  let test_step : t_test_step = test_start(status, "cur_shape_choice structural")
  and all_shapes : t_shape t_array = {len = 3 ; value = [| init_sh011() ; init_sh112() ; init_sh211() |]}
  and all_colors : t_color t_array = {len = 9; value=[| black ; white ; blue ; red ; green ; yellow ; cyan ; magenta ; grey |]}
  in
  let is_t_shape(shapes, result: t_shape t_array * 'a): bool =
    let shapes_list : t_shape list = list_of_array(shapes.value) in
    list_contains_value(shapes_list, all_shapes.value.(result))
  in
  (
    let test_result : t_cur_shape t_test_result = test_exec(test_step, cur_shape_choice, (all_shapes, 10, 20, all_colors))in
    (
      if test_is_success(test_result)
      then
        (
          assert_true(test_step, "la forme est bien comprise dans le tableau",is_t_shape(all_shapes, !(getintShape(test_get(test_result)))))
        )
      else test_error(test_step);
      test_end(test_step)
    )
  )
;;

(* -----------------------*)
(*  test de : _init_play  *)
(* -----------------------*)

(**
fonction de test fonctionnel de la  fonction init_play qui ouvre l'environement de jeu et choisi une premiere forme alleatoirement
@param status recupere l'environnement de test
@author LOUIS
 *)
let test_init_play_fonctional_1(status : t_test_status) : unit =
  let test_step : t_test_step = test_start(status, "init_play fonctional 1") in
  (
    let test_result : t_play t_test_result = test_exec(test_step, init_play, ()) in
    (
      if test_is_success(test_result)
      then
        (
          set_color(black);
          print_string("Voyez vous l'espace de jeu totalement vierge ? (oui/non)");
          let reponse_11 : string = read_line() in
          assert_equals(test_step, "Ouverture de l'espace de jeu", reponse_11, "oui")
        )
      else test_error(test_step);
      test_end(test_step);
      clear_graph();
      set_color(black);
    )
  )
;;

(* ------------------*)
(*  test de : insert *)
(* ------------------*)

(**
fonction de test fonctionnel de la  fonction invert_aux qui verifie si une forme a la place d'apparaitre a l'ecrant et qui l'insert.
@param status recupere l'environnement de test
@author LOUIS
 *)
let test_insert_fonctional_1(status : t_test_status) : unit =
  let test_step : t_test_step = test_start(status, "insert fonctional 1")
  and cur_1 : t_cur_shape =
    {
      base = ref {x = 7; y = 28};
      shape = ref 0;
      color = ref green
    }
  and shape_1 : t_point list = getShape(init_sh011())
  and param_1 : t_param = init_param()
  and my_mat_1 : t_color matrix = mat_make(15,28,white)
  in
  (
    let test_result : bool t_test_result = test_exec(test_step, insert, (cur_1, shape_1, param_1, my_mat_1)) in
    (
      if test_is_success(test_result)
      then
        (
          assert_equals(test_step, "la forme peut etre inseree", test_get(test_result), true)
        )
      else test_error(test_step);
      test_end(test_step)
    )
  )
;;

(* ---------------------------- *)
(*     fonction de test         *)
(* ---------------------------- *)

let test_run() : unit =
  let alltests : t_test_status = create_test_status() in
  (
    open_graph(452,800);
    test_convert_fonctional_1(alltests);
    (*question1*)
    test_draw_absolute_pt_functional_1(alltests);
    test_fill_absolute_pt_functional_1(alltests);
    test_drawfill_absolute_pt_functional_1(alltests);
    (*question2*)
    test_draw_relative_pt_functional_1(alltests);
    test_fill_relative_pt_functional_1(alltests);
    test_drawfill_relative_pt_functional_1(alltests);
    (*question3*)
    test_draw_pt_list_functional_1(alltests);
    test_fill_pt_list_functional_1(alltests);
    test_drawfill_pt_list_functional_1(alltests);
    (*question4*)
    test_draw_frame_functional_1(alltests);
    (*question 6*)
    test_getArrLen_functionnal(alltests);
    (*question 7*)
    test_color_choice_structural(alltests);
    test_cur_shape_choice_structural(alltests);
    test_init_play_fonctional_1(alltests);
    test_insert_fonctional_1(alltests);

    (*print des resultats de test DOIT RESTER A LA FIN !!!*)
    print_test_report(alltests)
  )
;;
