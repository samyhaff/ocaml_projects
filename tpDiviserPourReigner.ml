let rec nbInversionsAux l =
  |t1::t2::[] -> if t1 > t2 then 1
  |t1::t2::[] -> 0
  |t1::t2::q -> if t1 > t2 then 1 + nbInversions t2::q
  |t1::t2::q -> nbInversions t2::q;;

let rec nbInversions l =
  |t1::t2::[] -> if t1 > t2 then 1
  |t1::t2::[] -> 0
  |t::q -> nbInversions q + nbInversionsAux t::q;;

let decoupe l =
  let rec decoupeAux l acc l1 l2= match l with
  |[] -> l1, l2
  |t::q -> if acc <= (List.length l) / 2 then decoupeAux q (acc + 1) t::1 l2
  |t::q -> decoupeAux q (acc + 1) l1 t::l2
  in decoupeAux l 0 [] [];;
