let board = Othello.init_board

let target =
  ( [|
      [| 0; 0; 0; 0; 0; 0; 0; 0 |];
      [| 0; 0; 0; 0; 0; 0; 0; 0 |];
      [| 0; 0; 0; 0; 0; 0; 0; 0 |];
      [| 0; 0; 0; 2; 1; 0; 0; 0 |];
      [| 0; 0; 0; 1; 2; 0; 0; 0 |];
      [| 0; 0; 0; 0; 0; 0; 0; 0 |];
      [| 0; 0; 0; 0; 0; 0; 0; 0 |];
      [| 0; 0; 0; 0; 0; 0; 0; 0 |];
    |],
    1 )
;;

assert (board = target);;

let afficher_etat (plateau, _) =
  let char_from_jeton jeton =
    match jeton with
    | 0 -> '.'
    | 1 -> 'X'
    | 2 -> 'O'
    | _ -> failwith "jeton < 0 ou > 2"
  in
  let l = ref 0 in
  print_string "  ";
  Array.iter print_int [| 0; 1; 2; 3; 4; 5; 6; 7 |];
  print_newline ();
  Array.iter
    (fun ligne ->
      print_int !l;
      incr l;
      print_string " ";
      Array.iter (fun case -> print_char (char_from_jeton case)) ligne;
      print_newline ())
    plateau
  (*let print_list l =
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
  print_int j*)
  (* let () = print_bool (est_coup_possible etat 3 3 ) *)
  (* let () = print_othellier (jouer etat (2,2)) *)
  (* let () = print_bool (p2.(2).(5) = 2)  ?? *)
  (* let () = print_int (minmax etat2 1) ; print_char ';'
let () = print_coup (strategie_minmax etat2 1) *)
  (* let etat3 = jouer etat2 (5,5) *)
  (* let () = print_othellier etat3 *)
  (* let () = print_list (ensemble_coups_possibles etat3) *)
  (* let () = print_coup (strategie etat2) *)

  (*let board = ;;*)
in

let board = Othello.play board (5, 4) in
let board = Othello.play board (3, 2) in
let board = Othello.play board (3, 1) in
afficher_etat board;
let target =
  ( [|
      [| 0; 0; 0; 0; 0; 0; 0; 0 |];
      [| 0; 0; 0; 0; 0; 0; 0; 0 |];
      [| 0; 0; 0; 0; 0; 0; 0; 0 |];
      [| 0; 1; 1; 1; 1; 0; 0; 0 |];
      [| 0; 0; 0; 1; 1; 0; 0; 0 |];
      [| 0; 0; 0; 0; 1; 0; 0; 0 |];
      [| 0; 0; 0; 0; 0; 0; 0; 0 |];
      [| 0; 0; 0; 0; 0; 0; 0; 0 |];
    |],
    2 )
in
assert (board = target);
print_string "test passed"
