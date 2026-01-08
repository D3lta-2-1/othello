(*let base_score =
  [|
    5; 5; 5; 5; 5; 5; 5; 5;
    5; 4; 4; 4; 4; 4; 4; 5;
    5; 4; 3; 3; 3; 3; 4; 5;
    5; 4; 3; 2; 2; 3; 4; 5;
    5; 4; 3; 2; 2; 3; 4; 5;
    5; 4; 3; 3; 3; 3; 4; 5;
    5; 4; 4; 4; 4; 4; 4; 5;
    5; 5; 5; 5; 5; 5; 5; 5;
  |] [@@ocamlformat "disable"] *)

let agressive_on_corner =
  [|
    6; 5; 4; 3; 3; 4; 5; 6;
    5; 4; 3; 2; 2; 3; 4; 5;
    4; 3; 2; 1; 1; 2; 3; 4;
    3; 2; 1; 0; 0; 1; 2; 3;
    3; 2; 1; 0; 0; 1; 2; 3;
    4; 3; 2; 1; 1; 2; 3; 4;
    5; 4; 3; 2; 2; 3; 4; 5;
    6; 5; 4; 3; 3; 4; 5; 6;
  |] [@@ocamlformat "disable"]

type 'a iter_result = Continue | Stop of 'a

let break_iter (f : 'a -> 'b iter_result) vec =
  let i = ref 0 in
  let rec loop () =
    if !i >= Dynarray.length vec then None
    else
      match f (Dynarray.get vec !i) with
      | Continue ->
          incr i;
          loop ()
      | Stop y -> Some y
  in
  loop ()

let swap vec i j =
  let tmp = Dynarray.get vec i in
  Dynarray.set vec i (Dynarray.get vec j);
  Dynarray.set vec j tmp

let shuffle vec =
  for i = 0 to Dynarray.length vec - 1 do
    let j = Random.int (Dynarray.length vec - i) in
    swap vec i j
  done

let score goal board =
  let count token =
    match token with Othello.Empty -> 0 | c when c = goal -> 1 | _ -> -1
  in
  Array.fold_left (fun acc token -> acc + count token) 0 board

(* there might be a better way to do this *)
let corner_heuristic goal board =
  let i = ref 0 in
  let enum () =
    let i' = !i in
    incr i;
    i'
  in
  let count token =
    let value = agressive_on_corner.(enum ()) in
    match token with
    | Othello.Empty -> 0
    | c when c = goal -> value
    | _ -> -value
  in
  Array.fold_left (fun acc token -> acc + count token) 0 board

(* each token is rated between 1 and 5, 1 for being on the board, and +1 for each axis on which it can't be flipped *)

let degree_of_freedom_removed goal board =
  let rec travel pos direction right_token_encourted =
    let token = Othello.get board pos in
    match (token, Othello.try_get_case_from pos direction) with
    | Othello.Empty, _ -> (0, false)
    | t, Some next_pos when t = goal ->
        travel next_pos direction (right_token_encourted + 1)
    | _, Some next_pos ->
        let score, ended = travel next_pos direction 0 in
        (score + right_token_encourted, ended (* +1 for each right token *))
    | t, None when t = goal -> (0, true)
    | _, None -> (right_token_encourted, true)
  in

  let score = ref 0 in

  let count_on (start : int -> int * int) direction (restart : int -> int * int)
      =
    let rev_direction = Othello.rev direction in
    for i = 0 to Othello.board_size - 1 do
      let i_score, ended = travel (start i) direction 0 in
      score := !score + i_score;
      if not ended then
        let i_score, _ = travel (restart i) rev_direction 0 in
        score := !score + i_score
    done
  in

  count_on
    (fun i -> (i, 0))
    Othello.Right
    (fun i -> (i, Othello.board_size - 1));
  count_on (fun i -> (0, i)) Othello.Down (fun i -> (Othello.board_size - 1, i));

  count_on
    (fun i -> (i, 0))
    Othello.DownRight
    (fun i -> (Othello.board_size - 1, Othello.board_size - 1 - i));
  count_on
    (fun i -> (i, 0))
    Othello.UpRight
    (fun i -> (0, Othello.board_size - 1 - i));

  count_on
    (fun i -> (i, Othello.board_size - 1))
    Othello.DownLeft
    (fun i -> (Othello.board_size - 1, i));
  count_on
    (fun i -> (i, Othello.board_size - 1))
    Othello.UpLeft
    (fun i -> (0, i));

  !score

let freedom_heuristic goal board =
  corner_heuristic goal board
  + degree_of_freedom_removed goal board
  - degree_of_freedom_removed (Othello.other_player goal) board

(*If we are black, we want to maximize our score
If we are white, we want to minimize our score, so we must flip min and max functions,
it should return a move a the associated score ?
*)
let rec min_max_ab state (goal : Othello.token) depth (a : int) (b : int)
    (heuritistic : Othello.token -> Othello.token array -> int) =
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
      if Othello.player_turn state = goal then maximize else minimize
    in

    let moves = Othello.all_possible_moves state in
    let result =
      break_iter
        (fun move ->
          let v : int =
            min_max_ab (Othello.play state move) goal (depth - 1) !a !b
              heuritistic
          in
          counting_function v)
        moves
    in
    let r =
      match result with
      | Some v -> v
      | None when Othello.player_turn state = goal -> !a (*todo: check this*)
      | None -> !b
    in
    r
  end

(*first attempt, using score as an heuristic*)
let generic_minmax_strategy heuristic depth state =
  let state = Othello.from_ffi state in
  (*assume that we are playing this turn*)
  let goal = Othello.player_turn state in
  let moves = Othello.all_possible_moves state in
  shuffle moves;

  let evaluate_move move =
    min_max_ab (Othello.play state move) goal depth min_int max_int heuristic
  in

  let moves = Dynarray.map (fun move -> (move, evaluate_move move)) moves in
  let last = Dynarray.pop_last moves in
  let move, _ =
    Dynarray.fold_left
      (fun (m1, s1) (m2, s2) -> if s1 > s2 then (m1, s1) else (m2, s2))
      last moves
  in
  move

let random_strategy state =
  let moves = Othello.all_possible_moves state in
  Dynarray.get moves (Random.int (Dynarray.length moves))

(* use score as heuristic with 2 as max depth *)
let strategy1 = generic_minmax_strategy score 2

(* use score as heuristic with 3 as max depth *)
let strategy2 = generic_minmax_strategy score 3

(* use corner heuristic with 3 as max depth *)
let strategy3 = generic_minmax_strategy corner_heuristic 3

(* use corner heuristic with 4 as max depth *)
let strategy4 = generic_minmax_strategy corner_heuristic 4

(* use a new heuritisc this time *)
let strategy5 = generic_minmax_strategy freedom_heuristic 4
