let rec split l = match l with
  |[] -> [], []
  |t1::t2::q -> let (u, v) = split q in t1::u, t2::v
  |t::[] -> t::[], [];;

let rec maximum l = match l with
  |[] -> failwith "liste vide, pas de max"
  |t::[] -> t
  |l -> let l1, l2 = split l in max (maximum l1 maximum l2);;

let distance a b =
  let xA = a.(0) in
  let yA = a.(1) in
  let xB = b.(0) in
  let yB = b.(1) in
  sqrt ((xA -. xB) ** 2 +. (yA -. yB) ** 2);;

(* algo de force brute *)
