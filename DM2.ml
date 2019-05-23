open Array

(* polybome nul: [|0|] *)
(* polynome *)

let degre p =
  let n = Array.length p - 1 in
  let k = ref n in
  while p.(!k) = 0
  do
    k := !k - 1;
  done;
  !k;;

let agrandirRepr p m =
  let q = Array.create (m + 1) 0 in
  let n = degre p in
  for i = 0 to n
  do
    q.(i) <- p.(i);
  done;
  q;;

let rec reduireRepr p =
  let l = Array.length p in
  let n = ref l in
  if p = [|0|] then p
  else
  while p.(!n) = 0
  do
    n := !n - 1;
  done;
  begin
    Array.sub p 0 n;
  end;;
  p;;

let somme p q =
  let n = max (degre p) (degre q) in
  let s = Array.create n 0 in
  let p2 = agrandirRepr p n in
  let q2 = agrandirRepr q n in
  for i = 0 to n do
    s.(i) <- p2.(i) + q2.(i);
  done;
  s;;

(* n additions nécessaires pour 2 polynomes de taille n *)

let multiplicationNaive p q =
  let n = Array.length p in
  let m = Array.length q in
  let r = Array.create (n + m) 0 in
  let s = ref 0 in
  for k = 0 to m + n do
    begin
      for i = 0 to k do
        s := !s + (p.(i) * q.(k - i));
      done;
      r.(k) <- !s;
      s := 0;
    end;
  done;
  r;;

(* nb d'additions nécessaires: *)
(* nb de produits nécessaires: *)

let createPolynomes n m =
  let xn = Array.create (n + 1) 0 in
  let xm = Array.create (m + 1) 0 in
  begin
    xn.(n + 1) <- 1;
    xm.(m + 1) <- 1;
  end;
  (xm, xn);;

let karatsuba p q = match Array.length p with
  |1 -> [|p.(0), q.(0)|]
  |n -> let m = n / 2 in
    let xn, xm = creerPolynomes n m in
    let p1 = Array.sub p m m in
    let p2 = Array.sub p 0 m in
    let q1 = Array.sub q m m in
    let q2 = Array.sub q 0 m in
    let r1 = karatsuba p1 q1 in
    let r3 = karatsubda p2 q2 in
    let r2 = karatsuba (somme p1 p2) (somme q1 q2) in
    let xnr1 = multiplicationNaive r1 xn in
    let r4 = multiplicationNaive (somme (somme (r1  r2) -r3)) xm in
    let r = somme (somme xnr1 r3) r4 in
    r;;
