open Array;;

type partition = (int * int) array;;

let init n =
  let partition = Array.create n (0, 0) in
    for i = 0 to n - 1 do
      partition.(i) <- (i, 0)
    done;
  partition;;

let find1 p n =
  let i = ref n in
  while fst p.(!i) != i do
    i := fst p.(!i)
  done;
  !i;;

let rec find p n = match p.(n) with
  |(i, r) when i = fst p.(i) -> i
  |(i, r) -> let racine = find p i in p.(i) <- (racine, r); racine;;

let rec union p n m =
  let racine1 = find p n in
  let racine2 = find p m in
  let r1 = snd p.(n) in
  let r2 = snd p.(m) in
  if r1 > r2 then p.(racine2) <- (racine1, r2);
  if r2 < r1 then p.(racine1) <- (racine2, r1);
  if r1 = r2 then p.(racine1) <- (racine2, r1 + 1);;

type voisin = int list;;
type graphe = voisin array;;

let composantes g =
  let n = Array.length g in
  let p = init n in
  for i = 0 to n - 1 do
    for j = 0 to Array.lenth g.(i) do
      union p i j
    done
  done
p;;
