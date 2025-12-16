type othello = int array array * int 
type deplacement = H | HD | D | BD | B | BG | G | HG

let init_plat = 
  let tab = Array.make_matrix 8 8 0 in
  tab.(3).(3) <- 2 ; tab.(3).(4)<- 1 ; tab.(4).(3)<-1 ; tab.(4).(4)<-2 ; 
  (tab,1)

let est_position_valide i j =
  if i>=0 && i<=7 && j>=0 && j<=7 then true
  else false

let coup_direction i j d = 
  match d with
  |H-> (i-1),j
  |HG-> (i-1),(j-1)
  |G-> i,(j-1)
  |BG->(i+1),(j-1)
  |B->(i+1),j
  |BD->(i+1),(j+1)
  |D->i,(j+1)
  |HD->(i-1),(j+1)


let liste_directions = [H; HG; G; BG; B; BD; D; HD; H]

let est_coup_possible etat i j = 
  let p,index = etat in
  if not (est_position_valide i j && p.(i).(j) = 0) then false
  else begin 
  let b = ref false in
  let l = ref liste_directions in 
  while !l <> [] && not !b do 
    let d = List.hd !l in 
    l := List.tl !l ; 
    let i1,j1 = coup_direction i j d
   in 
    if est_position_valide i1 j1 && p.(i1).(j1) <> 0 then b := true
  done; !b end


let ensemble_coups_possibles etat =
  let l = ref [] in
  for i = 0 to 7 do 
    for j = 0 to 7 do 
      if est_coup_possible etat i j then
        l:= (i,j)::!l
    done;
  done;
  !l

let autre_joueur index = 2-index + 1

let copie_othello etat = 
  let p,index = etat in 
  let p1 = Array.make_matrix 8 8 0 in 
  for i = 0 to 7 do 
    for j=0 to 7 do 
      p1.(i).(j)<-p.(i).(j)
    done;
  done; (p1,index)

let rec retourner (p,index) l = 
  match l with 
  |[]-> ()
  |(i,j)::q-> assert(p.(i).(j) <>0); p.(i).(j) <- autre_joueur (p.(i).(j)) ; retourner (p,index) q

(* marche, normalement *)
let jouer (etat_base:othello) (i,j) = 
  let etat = copie_othello etat_base in 
  let p,index = etat in
  assert( i >= 0 && i <= 7 && j >=0 && j<=7 && p.(i).(j) = 0 && est_coup_possible etat i j);

  p.(i).(j)<- index ; 
  let l = ref liste_directions in
  let i1,j1 = ref i, ref j in
  while !l <> [] do 
    let d = List.hd !l in 
    l := List.tl !l;
    let liste_a_retourner = ref [] in
    let i_aux,j_aux = coup_direction i j d in
    i1:= i_aux ; j1:= j_aux ;
    while est_position_valide !i1 !j1 && p.(!i1).(!j1) = autre_joueur index do 
      (liste_a_retourner := (!i1,!j1)::!liste_a_retourner ; let i_aux1,j_aux1 = coup_direction !i1 !j1 d in 
      i1:=i_aux1 ; j1:=j_aux1)
    done;
    if est_position_valide !i1 !j1 && p.(!i1).(!j1) = index then 
      retourner etat !liste_a_retourner
    
  done; (p,autre_joueur index)

(* a améliorer*)
let heuristique etat = 
  let p,index = etat in 
  let s = ref 0 in 
  for i = 0 to 7 do
    for j = 0 to 7 do
      if p.(i).(j) = 1 then 
        s:= !s + 1 
      else if p.(i).(j) = 2 then 
        s:= !s - 1
      else ()
    done;
  done;
  !s

let score etat = 
  let p,index = etat in 
  let s = ref 0 in 
  for i = 0 to 7 do
    for j = 0 to 7 do
      if p.(i).(j) = 1 then 
        s:= !s + 1 
      else if p.(i).(j) = 2 then 
        s:= !s - 1
      else ()
    done;
  done;
  !s

(* renvoie 0 si la partie n'est pas terminée, 1 si noir a gagné, 2 si blanc a gagné *)
let est_partie_termine (p,index) = 
  let b = ref true in 
  for i=0 to 7 do
    for j = 0 to 7 do 
      if p.(i).(j) = 0 then 
        b := false
    done;
  done; !b

let rec minmax etat prof = 
  let p,index = etat in

  if est_partie_termine etat then 
    if score etat > 0 then 
      10000                           (* les valeurs du minmax sont comprises entre 0 et 8*8=64 0 à cause de l'heuristique donc on prend 100 comme l'infini *)                          (* les valeurs du minmax sont comprises entre 0 et 8*8=64 0 à cause de l'heuristique donc on prend 100 comme l'infini *)
    else if score etat < 0 then 
      -10000 
    else
      0
      
  else if prof = 0 then 
    heuristique etat
  else begin
    let l = ref (ensemble_coups_possibles etat) in
    let coup0 = List.hd !l in 
    let h_opt = ref (heuristique (jouer etat coup0)) in 

    while !l <> [] do 
      let coup = List.hd !l in 
      let nouvel_etat = jouer etat coup in 
      l := List.tl !l ; 
      let h = minmax nouvel_etat (prof - 1) in 
      if index = 1 then 
        (if h > !h_opt then 
          h_opt := h )
      else
        (if h < !h_opt then 
          h_opt := h )
    done; !h_opt
  end

let strategie etat =
  let init = List.map (fun coup -> (coup,minmax (jouer etat coup) 6)) (ensemble_coups_possibles etat) in
  let (coup,heuristique) = List. hd (List.fast_sort (fun x y -> if x = y then 0 else if x < y then -1 else 1) init) in
  coup
  
let strategie_minmax etat prof = 
  let p,index = etat in 
  let l = ref (ensemble_coups_possibles etat) in 
  let coup_opt = ref (List.hd !l) in
  let h_opt = ref (heuristique (jouer etat !coup_opt)) in 

  while !l <> [] do 
    let coup = List.hd !l in 
    let nouvel_etat = jouer etat coup in 
    l := List.tl !l ; 
    let h = minmax nouvel_etat (prof - 1) in 
    if index = 1 then 
      (if h > !h_opt then 
        (coup_opt := coup ; h_opt := h ))
    else 
      (if h < !h_opt then
        (coup_opt := coup ; h_opt := h ))
  done; !coup_opt  

let print_othellier etat = 
  let p,index = etat in
  for i = 0 to 7 do 
    for j = 0 to 7 do
      print_string "| " ; if p.(i).(j) = 0 then print_string "0 "
      else if p.(i).(j) = 1 then print_string "N " 
      else if p.(j).(j) = 2 then print_string "B "
      else (print_int (p.(i).(j)) ; print_char ' ') (* Il y a des erreurs d'affichage, je ne comprends pas (voir plus bas les tests) *)
    done;
    print_char '|' ; print_char '\n'
  done; print_char '\n'

let print_bool b = 
  if b then print_string "true"
  else print_string "false"

let print_list l = List.iter (fun (i,j)->print_int i ; print_string "," ; print_int j ; print_string " ; ") l

let print_coup (i,j) = print_int i ; print_char ',' ; print_int j


let etat = init_plat
let etat1 = jouer etat (2,2)
(* let () = print_bool (est_coup_possible etat 3 3 ) *)
(* let () = print_othellier (jouer etat (2,2)) *)

let etat2 = jouer etat1 (2,5)
let () = print_othellier etat2

(* let () = print_bool (p2.(2).(5) = 2)  ?? *)

(* let () = print_int (minmax etat2 1) ; print_char ';' 

let () = print_coup (strategie_minmax etat2 1) *)



(* let etat3 = jouer etat2 (5,5) *)
(* let () = print_othellier etat3 *)
(* let () = print_list (ensemble_coups_possibles etat3) *)

let () = print_coup (strategie etat2)