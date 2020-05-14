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

let rec eval_base b l = match l with
  | [] -> 0
  | h::t -> h + b * (eval_base b t) ;;

let fibo n =
  let rec aux n acc1 acc2 = match n with
    |0 -> acc1
    |1 -> acc2
    |n -> aux (n - 1) acc2 (acc1 + acc2)
  in aux n 0 1;;

let eval poly x = eval_base x poly;;

let recherche_dicho x tab =
  let rec aux i j = 
    let y = (i + j) / 2 in 
      if x > y then aux y j 
      else if x < y then aux i y 
      else i
  in aux 0 (Array.length tab);;

type arbre = Vide | Noeud of int * arbre * arbre;;

let rec max_min a = match a with
  | Vide -> min_int, max_int
  | Noeud(x,Vide, Vide) -> x,x
  | Noeud(x,g,d) -> let max_g,min_g = max_min g and max_d,min_d = max_min d in
        (max max_g (max max_d x), min min_g (min min_d x)) ;;

(* abr *)

let rec cherche_abr e a = match a with
  | Vide -> false
  | Noeud(x,g,d) when x = e -> true
  | Noeud(x,g,d) when x > e -> cherche_abr e g
  | Noeud(x,g,d) -> cherche_abr e d;;

let rec max_sum a = match a with
  | Vide -> 0
  | Noeud(x,g,d) -> x + max (max_sum g) (max_sum d);;

(* hierarchie *)

type arbre_expr = Value of int | Op_bin of string * arbre_expr * arbre_expr | Op_un of string * arbre_expr;;

(* parcours d'expression arithmétiques *)

let accessible g s =
  let n = Array.length g in
  let dejavu = Array.make n false in
  let rec visite l = match l with
    | [] -> ()
    | h::t when dejavu.(h) -> visite t
    | h::t -> dejavu.(h) <- true ; visite g.(h) ; visite t
  in visite [s] ; dejavu ;;













