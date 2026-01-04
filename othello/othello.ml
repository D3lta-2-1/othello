type token = White | Black | Empty

let get_token = function
  | 0 -> Empty
  | 1 -> Black
  | 2 -> White
  | _ -> assert false

let board_size = 8

type board = token array

let is_position_within_borders (i, j) =
  i >= 0 && i < board_size && j >= 0 && j < board_size

let get board (i, j) =
  assert (is_position_within_borders (i, j));
  board.(i + (j * board_size))

let set board (i, j) token =
  assert (is_position_within_borders (i, j));
  board.(i + (j * board_size)) <- token

let iterate (f : int * int -> unit) =
  for i = 0 to board_size - 1 do
    for j = 0 to board_size - 1 do
      f (i, j)
    done
  done

(* board + player turn *)
type othello = board * token

let init_othellier =
  let array = Array.make (board_size * board_size) Empty in
  set array (3, 3) White;
  set array (3, 4) Black;
  set array (4, 3) Black;
  set array (4, 4) White;
  (array, Black)

let from_ffi (board, player) =
  let get_token i =
    let x = i mod board_size in
    let y = i / board_size in
    get_token board.(x).(y)
  in
  let array = Array.init (board_size * board_size) get_token in
  let player = get_token player in
  assert (player <> Empty);
  (array, player)

(* let init_othellier =
  let array = Array.make (board_size * board_size) Empty in
  set array (3, 3) White;
  set array (3, 4) Black;
  set array (4, 3) Black;
  set array (4, 4) White;
  (array, Black) *)

type displacement =
  | Up
  | UpRight
  | Right
  | BottomRight
  | Bottom
  | BottomLeft
  | Left
  | UpLeft

let directions =
  [| Up; UpLeft; Left; BottomLeft; Bottom; BottomRight; Right; UpRight |]

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

let is_move_possible state pos =
  let board, _ = state in
  if not (is_position_within_borders pos && get board pos = Empty) then false
  else begin
    let b = ref false in
    Array.iter
      (fun d ->
        let neighboor = get_case_from pos d in
        if is_position_within_borders neighboor && get board neighboor <> Empty
        then b := true)
      directions;
    !b
  end

let all_possible_moves etat =
  let vec = Dynarray.create () in
  Dynarray.ensure_capacity vec 32;
  iterate (fun pos ->
      if is_move_possible etat pos then Dynarray.add_last vec pos);
  vec

let other_player = function
  | Empty -> assert false (* there is no other player for an empty token *)
  | White -> Black
  | Black -> White

let change_turn (board, player) = (board, other_player player)
let copy_state (board, player) = (Array.copy board, player)

let flip_token (board, player) l =
  assert (player <> Empty);
  List.iter (fun pos -> set board pos player) l

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
  let state = copy_state original_state in
  assert (is_position_within_borders pos && is_move_possible state pos);
  let board, player = state in
  set board pos player;
  Array.iter
    (fun d ->
      let cases_to_flip = find_cases_to_flip state pos d in
      flip_token state cases_to_flip)
    directions;
  change_turn state

let player_turn (_, player) = player
let board (board, _) = board

let is_game_over (board, _) =
  Array.fold_left (fun is_full token -> is_full && token <> Empty) true board
