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

let print_int_board =
  Array.iteri (fun i t ->
      print_int t;
      if i mod Othello.board_size = Othello.board_size - 1 then print_newline ()
      else print_string " ")

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

type freedom = Free | LockedByOtherToken | LockedByBorder

let count_locked goal board =
  (* assume this is already the right color *)
  let rec is_locked_in pos direction =
    match Othello.try_get_case_from pos direction with
    | None -> LockedByBorder
    | Some next_pos ->
        let token = Othello.get board next_pos in
        if token = Othello.Empty then Free
        else if token = goal then is_locked_in next_pos direction
        else LockedByBorder
  in

  let is_token_locked pos =
    if Othello.get board pos <> goal then Free
    else
      let paires =
        [
          (Othello.Up, Othello.Down);
          (Othello.Left, Othello.Right);
          (Othello.UpLeft, Othello.DownRight);
          (Othello.DownLeft, Othello.UpRight);
        ]
      in
      let rec locked_impl = function
        | [] -> LockedByBorder
        | (direction, rev) :: tail -> (
            match is_locked_in pos direction with
            | Free -> Free
            | LockedByBorder -> locked_impl tail
            | LockedByOtherToken ->
                let locked = is_locked_in pos rev in
                if locked = Free then Free
                else if locked_impl tail = Free then Free
                else locked)
      in
      locked_impl paires
  in

  let count = ref 0 in
  for i = 0 to (Othello.board_size * Othello.board_size) - 1 do
    match
      is_token_locked (i mod Othello.board_size, i / Othello.board_size)
    with
    | Free -> ()
    | LockedByBorder -> count := !count + 1
    | LockedByOtherToken -> ()
  done;

  !count

let partial_locked_heuritic goal board =
  count_locked goal board - count_locked (Othello.other_player goal) board

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

(* count locked tokens *)
let strategy5 = generic_minmax_strategy partial_locked_heuritic 4
