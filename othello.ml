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
  assert(est_position_valide i j && p.(i).(j) = 0);
  let b = ref false in
  let l = ref liste_directions in 
  while !l != [] && not !b do 
    let d = List.hd !l in 
    l := List.tl !l ; 
    let i1,j1 = coup_direction i j d in 
    if p.(i1).(j1) <> 0 then b := true
  done; !b 


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

let jouer (etat:othello) i j = 
  let p,index = etat in 
  assert( i >= 0 && i <= 7 && j >=0 && j<=7 && p.(i).(j) = 0 && est_coup_possible etat i j);

  let l = ref liste_directions in
  let i1,j1 = ref i, ref j in
  while !l != [] do 
    let d = List.hd !l in 
    let i_aux,j_aux = coup_direction i j d in
    i1:= i_aux ; j1:= j_aux ;
    while est_position_valide !i1 !j1 && p.(!i1).(!j1) = autre_joueur index do 
      (p.(!i1).(!j1)<- index ; let i_aux1,j_aux1 = coup_direction !i1 !j1 d in 
      i1:=i_aux1 ; j1:=j_aux1)
    done;
    l := List.tl !l
  done



let heuristique etat = 
  let s = ref 0 in 
  for i = 0 to 7 
    for j = 0 to 7 do
      if p.(i).(j) = 1 then 
        s:= !s + 1 
      else if p.(i).(j) = 2 then 
        s:= !s - 1
      else ()
    done;
  done;
  !s

let minmax etat = 
  let 
  



