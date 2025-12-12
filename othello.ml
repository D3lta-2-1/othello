type othello int array array * int 


let init_plat = 
  let tab = Array.make 8*8 0 in
  tab.(3).(3) <-
  (tab,1)

let jouer (etat:othello) i j = 
  let p,index = othello in 
  assert 
