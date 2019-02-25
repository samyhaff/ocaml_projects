open List;;

let l1 = [1; 2; 3];;
let l2 = 0::l1;;

let head liste = match liste with
  |[] -> failwith "liste vide"
  |t::q -> t;;

let queue liste = match liste with
  |[] -> failwith "liste vide"
  |t::q -> q;;

let rec keme liste k = match liste, k with
  |[], k -> failwith "pas de k-eme element"
  |t::q, 1 -> t
  |t::q, k -> keme q (k - 1);;

let rec avant_dernier l = match l with
  |[] -> failwith "liste vide"
  |[_] -> failwith "liste de longueur 1"
  |[a; b] -> a
  |_::q -> avant_dernier q;;

let rec mystere f liste = match liste with
  |[] -> []
  |t::q -> (f t) :: mystere f q;;

let f1 = function x -> x;;

let rec mystere f = function
  |[] -> []
  |t::q when f t > 1 -> t::(mystere f q)
  |t::q -> mystere f q;;

let rec vrai f = function
  |[] -> []
  |t::q when f t = true -> t::(vrai f q)
  |t::q -> vrai f q;;

let rec concatenation l1 l2 = match l1 with
  |[] -> l2
  |t::q -> t::(concatenation q l2);;

let rec exists f l = match l with
  |[] -> false
  |t::q when f t = true -> true
  |t::q -> exists f q;;

let rec pourTout f l = match l with
  |[] -> true
  |t::q when f t = False -> false
  |t::q -> f q;;
