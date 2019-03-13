open List;;

let l1 = [1; 2; 3];;
let l2 = 0::l1;;

let head liste = match liste with
  |[] -> failwith "liste vide"
  |t::q -> t;;

let queue liste = match liste with
  |[] -> failwith "liste vide"
  |t::q -> q;;

(* exercice 1 *)

let rec keme liste k = match liste, k with
  |[], k -> failwith "pas de k-eme element"
  |t::q, 1 -> t
  |t::q, k -> keme q (k - 1);;

let rec avant_dernier l = match l with
  |[] -> failwith "liste vide"
  |[_] -> failwith "liste de longueur 1"
  |[a; b] -> a
  |_::q -> avant_dernier q;;

let indice liste x =
  let rec indiceAux liste k x = match liste, k with
    |[], k -> failwith "élement absent de la liste..."
    |t::q, k when t = x -> k
    |t::q, k -> indiceAux q (k + 1) x
  in indiceAux liste 0 x;;

let inverse liste =
  let rec inverseAux liste acc = match liste with
    |[] -> acc
    |t::q -> inverseAux q (t::acc)
  in inverseAux liste [];;

let maMap f liste =
  let rec maMapAux f liste acc = match liste with
    |[] -> acc
    |t::q -> maMapAux f q ((f t)::acc)
  in maMapAux f liste [];;

(* exercice 2 *)

let premier liste = match liste with
  |[] -> failwith "liste vide..."
  |t::q -> t;;

let somme liste =
  let rec sommeAux liste acc = match liste with
    |[] -> acc
    |t::q -> sommeAux q (t + acc)
  in sommeAux liste 0;;
(* ou *)
let rec somme liste = match liste with
  |[] -> failwith "liste vide"
  |[a] -> a
  |t::q -> t + somme(q);;

let rec dernierElement liste =
  premier (inverse liste);;

let rec ajouterEnQueue liste x =
  inverse (x::(inverse liste));;

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

(*  exercice 5 *)

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

let rec purge l = match l with
  |[] -> []
  |t::q when mem t q = true -> purge q
  |t::q -> t::(purge q);;

let rec eliminate a l = match l with
  |[] -> []
  |t::q when t = a -> eliminate q
  |t::q -> t::(eliminate a q);;

let rec purge l = match l with
  |[] -> []
  |t::q -> t::(eliminate t (purge q));;

(* ensembles *)

let rec appartient x l = match l with
  |[] -> false
  |t::q when t = x -> true
  |t::q -> appartient x q;;

let rec union l1 l2 = match l1, l2 with
  |[], a -> a
  |t::q, a when (appartient t a) = false -> t::(union q a)
  |t::q, a -> union q a;;

let rec intersection l1 l2 = match l1, l2 with
  |[], a -> []
  |t::q, a when appartient t a -> t::(intersection q a)
  |t::q, a -> intersection q a;;

let rec concatenation l1 l2 = match l1, l2 with
  |[], a -> a
  |t::q, a -> t::(concatenation q a);;

(* insertion sort *)

let rec insere x l = match l with
  |[] -> [x]
  |t::q when x >= t -> t::(x::q)
  |t::q -> insere x q;;

let rec insertion l = match l with
  |[] -> []
  |t::q -> insere t (insertion q);;

(* selection sort *)

let rec minimun l = match l with
  |[] -> failwith "liste vide"
  |[a] -> a
  |t::q -> min t (minimun q);;

let rec select m l = match l with
  |[] -> []
  |t::q when t = m -> select m q
  |t::q -> t::select m q;;

let rec selection l = match l with
  |[] -> []
  |a -> (minimun l)::(selection (select (minimun l) l));;
