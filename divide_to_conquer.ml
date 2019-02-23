let rec fusion l1 l2 = match l1, l2 with
  |[], l2 -> l2
  |l1, [] -> l1
  |t1::q1, t2::q2 -> if t1 > t2 then t1::(fusion q1 l2) else t2::(fusion l1 q2);;

let rec split l = match l with
  |[] -> [], []
  |t1::[] -> [t1], []
  |t::q -> let t1, t2 = split q in t::t2, t1;;

let rec triFusion l = match l with
  |[] -> []
  |t1::[] -> [t1]
  |l -> let l1, l2 = split l in fusion ( triFusion l1) ( triFusion l2);;

let rec splitV2 l = match l with
  |[] -> [], []
  |t1::[] -> [t1], []
  |t1::t2::q -> let a,b = split q in t1::a, t2::b;;
