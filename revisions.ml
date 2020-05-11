let rec list_map f l = match l with
  |[] -> []
  |t::q -> (f t)::(list_map f q);;

let rec list_select f l = match l with 
  |[] -> []
  |t::q when f t -> t::(list_select f q)
  |t::q -> list_select f q;;

let rec insere x l = match l with 
  |[] -> [x]
  |t::_ when x <= t -> x::l
  |t::q -> t::(insere x q);;

let rec tri_insertion l = match l with 
  |[] -> []
  |t::q -> insere t (tri_insertion q);;

(* tri rapide *)

let list_rev l = 
  let rec aux l acc = match l with
    |[] -> acc
    |t::q -> aux q (t::acc) 
  in aux l [];;

(* exp rapide *)

let rec puissance x n = match n with
  |0 -> 1
  |k -> x * (puissance x (k - 1));;

let rec pgcd a b = match a,b with
  |x, y when y > x -> pgcd y x
  |x, 0 -> x
  |x, y -> pgcd y (x mod y);;

let base x n = 
  let rec aux x acc = match x with 
    |0 -> acc
    |x -> aux (x / n) ((x mod n)::acc)
  in aux x [];;

base 13 2;;

let fibo n =
  let rec aux n acc1 acc2 = match n with
    |0 -> acc1
    |1 -> acc2
    |n -> aux (n - 1) acc2 (acc1 + acc2)
  in aux n 0 1;;

(* horner *)

let recherche_dicho x tab =
  let rec aux i j = 
    let y = (i + j) / 2 in 
      if x > y then aux y j 
      else if x < y then aux i y 
      else i
  in aux 0 (Array.length tab);;

type arbre = Vide | Noeud of int * arbre * arbre;;

let rec max_arbre a = match a with 
  |Vide -> failwith "abre vide" 
  |Noeud(x, Vide, Vide) -> x
  |Noeud(x, g, d) -> 
      let a = max_arbre g in 
      let b = max_arbre d in 
        max a b;;
















