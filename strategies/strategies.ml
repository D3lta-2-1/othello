let base_score =
  [|
    [| 5; 5; 5; 5; 5; 5; 5; 5 |];
    [| 5; 4; 4; 4; 4; 4; 4; 5 |];
    [| 5; 4; 3; 3; 3; 3; 4; 5 |];
    [| 5; 4; 3; 2; 2; 3; 4; 5 |];
    [| 5; 4; 3; 2; 2; 3; 4; 5 |];
    [| 5; 4; 3; 3; 3; 3; 4; 5 |];
    [| 5; 4; 4; 4; 4; 4; 4; 5 |];
    [| 5; 5; 5; 5; 5; 5; 5; 5 |];
  |]

let agressive_on_corner =
  [|
    [| 6; 5; 4; 3; 3; 4; 5; 6 |];
    [| 5; 4; 3; 2; 2; 3; 4; 5 |];
    [| 4; 3; 2; 1; 1; 2; 3; 4 |];
    [| 3; 2; 1; 0; 0; 1; 2; 3 |];
    [| 3; 2; 1; 0; 0; 1; 2; 3 |];
    [| 4; 3; 2; 1; 1; 2; 3; 4 |];
    [| 5; 4; 3; 2; 2; 3; 4; 5 |];
    [| 6; 5; 4; 3; 3; 4; 5; 6 |];
  |]

type 'a iter_result = Continue | Stop of 'a

let break_iter (f : 'a -> 'b iter_result) =
  let rec loop = function
    | [] -> None
    | x :: xs -> ( match f x with Continue -> loop xs | Stop y -> Some y)
  in
  loop

(*allow us to choose for which player we are trying to maximize*)
type goal = Max | Min

let score (goal : int -> goal) board =
  let s = ref 0 in

  let count pos =
    let piece = Othello.get board pos in
    if piece <> 0 then
      match goal piece with Max -> s := !s + 1 | Min -> s := !s - 1
  in
  Othello.iterate count;
  !s

let corner_heuristic (goal : int -> goal) board =
  let s = ref 0 in
  let count pos =
    let piece = Othello.get board pos in
    let score = Othello.get agressive_on_corner pos in
    if piece <> 0 then
      match goal piece with Max -> s := !s + score | Min -> s := !s - score
  in
  Othello.iterate count;
  !s

(*If we are black, we want to maximize our score
If we are white, we want to minimize our score, so we must flip min and max functions,
it should return a move a the associated score ?
*)
let rec min_max_ab state (goal : int -> goal) depth a b heuritistic =
  if Othello.is_game_over state then score goal (Othello.board state)
    (* we don't want to just win, we want to win with the most pieces *)
  else if depth = 0 then heuritistic goal (Othello.board state)
  else begin
    let a = ref a in
    let b = ref b in

    let maximize v =
      if v >= !b then Stop v
      else begin
        a := max !a v;
        Continue
      end
    in

    let minimize v =
      if v <= !a then Stop v
      else begin
        b := min !b v;
        Continue
      end
    in

    let counting_function =
      match goal (Othello.player_turn state) with
      | Max -> maximize
      | Min -> minimize
    in

    let moves = Othello.all_possible_moves state in
    let result =
      break_iter
        (fun move ->
          let v =
            min_max_ab (Othello.play state move) goal (depth - 1) !a !b
              heuritistic
          in
          counting_function v)
        moves
    in
    match result with
    | Some v -> v
    | None when Othello.player_turn state = Othello.black -> !a
    | None when Othello.player_turn state = Othello.white -> !b
    | None -> assert false (*should be unreachable*)
  end

let maximize_for_black = function
  | 1 -> Max
  | 2 -> Min
  | _ -> assert false (*black is 1*)

let maximize_for_white = function
  | 1 -> Min
  | 2 -> Max
  | _ -> assert false (*white is 2*)

let use_corresponding_goal = function
  | 1 -> maximize_for_black
  | 2 -> maximize_for_white
  | _ -> assert false

let random_strategy state =
  let moves = Othello.all_possible_moves state in
  List.nth moves (Random.int (List.length moves))

(*first attempt, using score as an heuristic*)
let generic_minmax_strategy heuristic depth state =
  (*assume that we are playing this turn*)
  let player_turn = Othello.player_turn state in
  let goal = use_corresponding_goal player_turn in
  let moves = Othello.all_possible_moves state in

  let evaluate_move move =
    min_max_ab (Othello.play state move) goal depth min_int max_int heuristic
  in

  let moves = List.map (fun move -> (move, evaluate_move move)) moves in

  let move, _ =
    match moves with
    | [] -> assert false (* no moves available *)
    | t :: q ->
        List.fold_left
          (fun (m1, s1) (m2, s2) -> if s1 > s2 then (m1, s1) else (m2, s2))
            (* always choose the move with the highest score *)
          t q
  in
  move

(* use score as heuristic with 4 as max depth *)
let strategy1 = generic_minmax_strategy score 3

(* use score as heuristic with 2 as max depth *)
let strategy2 = generic_minmax_strategy score 2

(* use corner heuristic with 4 as max depth *)
let strategy3 = generic_minmax_strategy corner_heuristic 3

(*
let heuritistic (board, _) =
  let s = ref 0 in

  let count pos =
    if Othello.get board pos = 1 then s := !s + 1
    else if Othello.get board pos = 2 then s := !s - 1
    else ()
  in
  Othello.iterate count;
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
  !coup_opt*)
