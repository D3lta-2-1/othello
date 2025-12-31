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

let init_board =
  let tab = Array.make_matrix board_size board_size 0 in
  tab.(3).(3) <- white;
  tab.(3).(4) <- black;
  tab.(4).(3) <- black;
  tab.(4).(4) <- white;
  (tab, 1)

let iterate (f : int -> int -> unit) =
  for i = 0 to board_size - 1 do
    for j = 0 to board_size - 1 do
      f i j
    done
  done

let is_position_valid i j =
  if i >= 0 && i < board_size && j >= 0 && j < board_size then true else false

let get_case_from i j d =
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

let is_move_possible state i j =
  let p, _ = state in
  if not (is_position_valid i j && p.(i).(j) = 0) then false
  else begin
    let b = ref false in
    let l = ref directions in
    while !l <> [] && not !b do
      let d = List.hd !l in
      l := List.tl !l;
      let i1, j1 = get_case_from i j d in
      if is_position_valid i1 j1 && p.(i1).(j1) <> 0 then b := true
    done;
    !b
  end

let all_possible_moves etat =
  let l = ref [] in
  for i = 0 to 7 do
    for j = 0 to 7 do
      if is_move_possible etat i j then l := (i, j) :: !l
    done
  done;
  !l

let other_player = function
  | c when c = black -> white
  | c when c = white -> black
  | _ -> assert false

let copy_board etat =
  let p, index = etat in
  let p1 = Array.make_matrix 8 8 0 in
  iterate (fun i j -> p1.(i).(j) <- p.(i).(j));
  (p1, index)

let rec flip (p, index) l =
  match l with
  | [] -> ()
  | (i, j) :: q ->
      assert (p.(i).(j) <> 0);
      p.(i).(j) <- other_player p.(i).(j);
      flip (p, index) q

(* marche, normalement *)
let play (original_state : othello) (i, j) =
  let state = copy_board original_state in
  let p, index = state in
  assert (
    i >= 0 && i < board_size && j >= 0 && j < board_size
    && p.(i).(j) = 0
    && is_move_possible state i j);

  p.(i).(j) <- index;
  let l = ref directions in
  let i1, j1 = (ref i, ref j) in
  while !l <> [] do
    let d = List.hd !l in
    l := List.tl !l;
    let liste_a_retourner = ref [] in
    let i_aux, j_aux = get_case_from i j d in
    i1 := i_aux;
    j1 := j_aux;
    while is_position_valid !i1 !j1 && p.(!i1).(!j1) = other_player index do
      liste_a_retourner := (!i1, !j1) :: !liste_a_retourner;
      let i_aux1, j_aux1 = get_case_from !i1 !j1 d in
      i1 := i_aux1;
      j1 := j_aux1
    done;
    if is_position_valid !i1 !j1 && p.(!i1).(!j1) = index then
      flip state !liste_a_retourner
  done;
  (p, other_player index)

(* renvoie 0 si la partie n'est pas terminée, 1 si noir a gagné, 2 si blanc a gagné *)
let is_game_over (p, _) =
  let b = ref true in
  iterate (fun i j -> if p.(i).(j) = 0 then b := false);
  !b
