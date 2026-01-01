(* board + player turn *)
type othello = int array array * int

type displacement =
  | Up
  | UpRight
  | Right
  | BottomRight
  | Bottom
  | BottomLeft
  | Left
  | UpLeft

let board_size = 8
let white = 2
let black = 1
let empty = 0

let init_board =
  let tab = Array.make_matrix board_size board_size 0 in
  tab.(3).(3) <- white;
  tab.(3).(4) <- black;
  tab.(4).(3) <- black;
  tab.(4).(4) <- white;
  (tab, 1)

let iterate (f : int * int -> unit) =
  for i = 0 to board_size - 1 do
    for j = 0 to board_size - 1 do
      f (i, j)
    done
  done

let is_position_within_borders (i, j) =
  if i >= 0 && i < board_size && j >= 0 && j < board_size then true else false

let get board (i, j) = board.(i).(j)

let get_case_from (i, j) d =
  match d with
  | Up -> (i - 1, j)
  | UpLeft -> (i - 1, j - 1)
  | Left -> (i, j - 1)
  | BottomLeft -> (i + 1, j - 1)
  | Bottom -> (i + 1, j)
  | BottomRight -> (i + 1, j + 1)
  | Right -> (i, j + 1)
  | UpRight -> (i - 1, j + 1)

let directions =
  [ Up; UpLeft; Left; BottomLeft; Bottom; BottomRight; Right; UpRight ]

let is_move_possible state pos =
  let board, _ = state in
  if not (is_position_within_borders pos && get board pos = empty) then false
  else begin
    let b = ref false in
    List.iter
      (fun d ->
        let neighboor = get_case_from pos d in
        if is_position_within_borders neighboor && get board neighboor <> empty
        then b := true)
      directions;
    !b
  end

let all_possible_moves etat =
  let l = ref [] in
  iterate (fun pos -> if is_move_possible etat pos then l := pos :: !l);
  !l

let other_player = function
  | c when c = black -> white
  | c when c = white -> black
  | _ -> assert false

let change_turn (board, player) = (board, other_player player)

let copy_board etat =
  let board, player = etat in
  (Array.map Array.copy board, player)

let flip (board, player) l = List.iter (fun (i, j) -> board.(i).(j) <- player) l

let find_cases_to_flip (board, player) pos d =
  let cases = ref [] in
  let next_case = ref (get_case_from pos d) in
  let other_color = other_player player in
  while
    is_position_within_borders !next_case && get board !next_case == other_color
  do
    cases := !next_case :: !cases;
    next_case := get_case_from !next_case d
  done;
  if is_position_within_borders !next_case && get board !next_case = player then
    !cases
  else []

let play (original_state : othello) pos =
  let state = copy_board original_state in
  assert (is_position_within_borders pos && is_move_possible state pos);
  let i, j = pos in
  let board, player = state in
  board.(i).(j) <- player;
  List.iter
    (fun d ->
      let cases_to_flip = find_cases_to_flip state pos d in
      flip state cases_to_flip)
    directions;
  change_turn state

let player_turn (_, player) = player

let is_game_over (board, _) =
  let b = ref true in
  iterate (fun pos -> if get board pos = 0 then b := false);
  !b
