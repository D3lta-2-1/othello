type othello int array array * int 


let init_plat = 
  let tab = Array.make 8*8 0 in
  tab.(3).(3) <- 2 ; tab.(3).(4)<- 1 ; tab.(4).(3)<-1 ; tab.(4).(4)<-2
  (tab,1)

let jouer (etat:othello) i j = 
  let p,index = othello in 
  assert( i >= 0 && i <= 7 && j >=0 && j<=7);
  let 







let ensemble_coups_possibles othello =
  ()