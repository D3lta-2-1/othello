(* a améliorer*)
(* let base_score =
  [|
    [|5;5;5;5;5;5;5;5|];
    [|5;4;4;4;4;4;4;5|];
    [|5;4;3;3;3;3;4;5|];
    [|5;4;3;2;2;3;4;5|];
    [|5;4;3;2;2;3;4;5|];
    [|5;4;3;3;3;3;4;5|];
    [|5;4;4;4;4;4;4;5|];
    [|5;5;5;5;5;5;5;5|]
  |] *)

let heuritistic (board, _) =
  let s = ref 0 in

  let evaluate_at pos =
    if Othello.get board pos = 1 then s := !s + 1
    else if Othello.get board pos = 2 then s := !s - 1
    else ()
  in
  Othello.iterate evaluate_at;
  !s

let score (board, _) =
  let s = ref 0 in

  let evaluate_at pos =
    if Othello.get board pos = 1 then s := !s + 1
    else if Othello.get board pos = 2 then s := !s - 1
    else ()
  in
  Othello.iterate evaluate_at;
  !s

let rec minmax state depth =
  let _, index = state in

  if Othello.is_game_over state then
    if score state > 0 then max_int else if score state < 0 then min_int else 0
  else if depth = 0 then heuritistic state
  else begin
    let l = ref (Othello.all_possible_moves state) in
    let coup0 = List.hd !l in
    let h_opt = ref (heuritistic (Othello.play state coup0)) in

    while !l <> [] do
      let coup = List.hd !l in
      let nouvel_etat = Othello.play state coup in
      l := List.tl !l;
      let h = minmax nouvel_etat (depth - 1) in
      if index = 1 then h_opt := max !h_opt h else h_opt := min !h_opt h
    done;
    !h_opt
  end

(* let strategie etat =
  let init = List.map (fun coup -> (coup,minmax (jouer etat coup) 3)) (ensemble_coups_possibles etat) in
  let (coup,heuristique) = List. hd (List.fast_sort (fun x y -> if x = y then 0 else if x < y then -1 else 1) init) in
  coup *)

let minmax_strategy depth state =
  let _, index = state in
  let l = ref (Othello.all_possible_moves state) in
  let coup_opt = ref (List.hd !l) in
  let h_opt = ref (heuritistic (Othello.play state !coup_opt)) in

  while !l <> [] do
    let coup = List.hd !l in
    let nouvel_etat = Othello.play state coup in
    l := List.tl !l;
    let h = minmax nouvel_etat (depth - 1) in
    if index = 1 then (
      if h > !h_opt then (
        coup_opt := coup;
        h_opt := h))
    else if h < !h_opt then (
      coup_opt := coup;
      h_opt := h)
  done;
  !coup_opt

(* coplexite : O(sqrt(n))*)
let edge_counter (board, _) =
  let s = ref 0 in
  for i = 0 to 7 do
    s := !s + (((-2 * board.(i).(7)) + 3) * board.(i).(7))
    (*fait -1 si la case est possede par le joueur 2, 1 si possédé par le joueur 1 et 0 sinon*)
  done;
  for i = 0 to 7 do
    s := !s + (((-2 * board.(i).(0)) + 3) * board.(i).(0))
    (*on compte deux fois les coins car ce sont des positions fortes*)
  done;
  !s

let rec minmax_profondeur_dynamique depth state : int =
  if depth = 0 || Othello.is_game_over state then heuritistic state
  else
    let _, index = state in
    let l = ref (Othello.all_possible_moves state) in
    let coup_opt = ref (List.hd !l) in
    let h_opt = ref (heuritistic (Othello.play state !coup_opt)) in
    while !l <> [] do
      let coup = List.hd !l in
      let nouvel_etat = Othello.play state coup in
      l := List.tl !l;
      let stabilite = edge_counter state in
      let h =
        if index = 1 then
          if stabilite < 0 then minmax_profondeur_dynamique depth nouvel_etat
          else minmax_profondeur_dynamique (depth - 1) nouvel_etat
        else if stabilite > 0 then minmax_profondeur_dynamique depth nouvel_etat
        else minmax_profondeur_dynamique (depth - 1) nouvel_etat
      in
      if index = 1 then
        if h > !h_opt then (
          coup_opt := coup;
          h_opt := h)
        else if h < !h_opt then (
          coup_opt := coup;
          h_opt := h)
    done;
    !h_opt

let strategie_minmax_dynamique depth state =
  let _, index = state in
  let l = ref (Othello.all_possible_moves state) in
  let optimal_move = ref (List.hd !l) in
  let h_opt = ref (heuritistic (Othello.play state !optimal_move)) in

  while !l <> [] do
    let move = List.hd !l in
    let nouvel_etat = Othello.play state move in
    l := List.tl !l;
    let h = minmax_profondeur_dynamique (depth - 1) nouvel_etat in
    if index = 1 then (
      if h > !h_opt then (
        optimal_move := move;
        h_opt := h))
    else if h < !h_opt then (
      optimal_move := move;
      h_opt := h)
  done;
  !optimal_move

let rec minmax_ab depth state a b =
  let _, index = state in

  if Othello.is_game_over state then
    if score state > 0 then max_int else if score state < 0 then max_int else 0
  else if depth = 0 then heuritistic state
  else begin
    let a_aux = ref a in
    let b_aux = ref b in
    let l = ref (Othello.all_possible_moves state) in
    let coup0 = List.hd !l in
    let h_opt = ref (heuritistic (Othello.play state coup0)) in

    let doit_continuer = ref true in
    while !doit_continuer && !l <> [] do
      let coup = List.hd !l in
      let nouvel_etat = Othello.play state coup in
      l := List.tl !l;
      let h = minmax_ab (depth - 1) nouvel_etat !a_aux !b_aux in
      if index = 1 then
        if h > !b_aux then (
          doit_continuer := false;
          h_opt := h)
        else (
          h_opt := max !h_opt h;
          a_aux := max !a_aux !h_opt)
      else if h < !a_aux then (
        doit_continuer := false;
        h_opt := h)
      else (
        h_opt := min !h_opt h;
        b_aux := min !b_aux !h_opt)
    done;
    !h_opt
  end

let strategie_minmax_ab depth state =
  let _, index = state in
  let l = ref (Othello.all_possible_moves state) in
  let optimal_move = ref (List.hd !l) in
  let h_opt = ref (heuritistic (Othello.play state !optimal_move)) in
  let a_aux = ref min_int in
  let b_aux = ref max_int in

  while !l <> [] do
    let move = List.hd !l in
    let new_state = Othello.play state move in
    l := List.tl !l;
    let h = minmax_ab (depth - 1) new_state !a_aux !b_aux in
    if index = 1 then (
      if !a_aux >= !b_aux then h_opt := max !h_opt h;
      a_aux := max !a_aux h)
    else (
      if !b_aux <= !a_aux then h_opt := min !h_opt h;
      b_aux := min !b_aux h)
  done;
  !optimal_move

let dict = Hashtbl.create 1000

let rec minmax_ab_memoisation etat prof a b =
  let _, index = etat in

  if Othello.is_game_over etat then
    if score etat > 0 then 10000
      (* les valeurs du minmax sont comprises entre 0 et 8*8=64 0 à cause de l'heuristique donc on prend 100 comme l'infini *)
    else if score etat < 0 then -10000
    else 0
  else if prof = 0 then heuritistic etat
  else begin
    match Hashtbl.find_opt dict etat with
    | Some x -> x
    | None -> begin
        let a_aux = ref a in
        let b_aux = ref b in
        let l = ref (Othello.all_possible_moves etat) in
        let coup0 = List.hd !l in
        let h_opt = ref (heuritistic (Othello.play etat coup0)) in

        let doit_continuer = ref true in
        while !doit_continuer && !l <> [] do
          let coup = List.hd !l in
          let nouvel_etat = Othello.play etat coup in
          l := List.tl !l;
          let h = minmax_ab_memoisation nouvel_etat (prof - 1) !a_aux !b_aux in
          if index = 1 then
            if h > !b_aux then (
              doit_continuer := false;
              h_opt := h)
            else (
              h_opt := max !h_opt h;
              a_aux := max !a_aux !h_opt)
          else if h < !a_aux then (
            doit_continuer := false;
            h_opt := h)
          else (
            h_opt := min !h_opt h;
            b_aux := min !b_aux !h_opt)
        done;
        Hashtbl.add dict etat !h_opt;
        !h_opt
      end
  end

let strategie_minmax_ab_memoisation prof etat =
  let _, index = etat in
  let l = ref (Othello.all_possible_moves etat) in
  let coup_opt = ref (List.hd !l) in
  let h_opt = ref (heuritistic (Othello.play etat !coup_opt)) in
  let a_aux = ref (-10000) in
  let b_aux = ref 10000 in

  while !l <> [] do
    let coup = List.hd !l in
    let nouvel_etat = Othello.play etat coup in
    l := List.tl !l;
    let h = minmax_ab (prof - 1) nouvel_etat !a_aux !b_aux in
    if index = 1 then (
      if h > !h_opt then (
        coup_opt := coup;
        h_opt := h;
        a_aux := max !a_aux h))
    else if h < !h_opt then (
      coup_opt := coup;
      h_opt := h;
      a_aux := min !a_aux h)
  done;
  !coup_opt

let print_othellier etat =
  let board, _ = etat in
  for i = 0 to 7 do
    for j = 0 to 7 do
      print_string "| ";
      if board.(i).(j) = 0 then print_string "0 "
      else if board.(i).(j) = 1 then print_string "N "
      else if board.(j).(j) = 2 then print_string "B "
      else (
        print_int board.(i).(j);
        print_char ' '
        (* Il y a des erreurs d'affichage, je ne comprends pas (voir plus bas les tests) *))
    done;
    print_char '|';
    print_char '\n'
  done;
  print_char '\n'

let print_bool b = if b then print_string "true" else print_string "false"

let print_list l =
  List.iter
    (fun (i, j) ->
      print_int i;
      print_string ",";
      print_int j;
      print_string " ; ")
    l

let print_coup (i, j) =
  print_int i;
  print_char ',';
  print_int j

let etat = Othello.init_board
let etat1 = Othello.play etat (2, 2)
(* let () = print_bool (est_coup_possible etat 3 3 ) *)
(* let () = print_othellier (jouer etat (2,2)) *)

let etat2 = Othello.play etat1 (2, 5)
let () = print_othellier etat2

(* let () = print_bool (p2.(2).(5) = 2)  ?? *)

(* let () = print_int (minmax etat2 1) ; print_char ';'

let () = print_coup (strategie_minmax etat2 1) *)
(* let etat3 = jouer etat2 (5,5) *)
(* let () = print_othellier etat3 *)
(* let () = print_list (ensemble_coups_possibles etat3) *)

(* let () = print_coup (strategie etat2) *)
