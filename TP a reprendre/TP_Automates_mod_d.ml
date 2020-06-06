(* TP AUTOMATES *)
(* question 1 *)

let rec binaire_faible n = match n with
    | 0 -> []
    | n -> (n mod 2)::(binaire_faible (n / 2)) ;;

let binaire_fort n = List.rev (binaire_faible n) ;;



let bin_fort n = 
let rec aux acc n = match n with
| 0 -> acc
| n -> aux ((n mod 2):: acc) (n/2)
in aux [] n;;



type ('a, 'b) afd = {init: 'a ;
                     accept: 'a list ;
                     delta: (('a * 'b) * 'a) list} ;;



let mon_test  = {	init = 2 ; 
					accept = [1;2;3] ; 
					delta = [ 	((2,0),2) ; ((2,1),1) ; ((1,0),3) ;((1,1),2)]} ;;
				

(* question 2 *)

let rec mem x l = match l with
|[] -> false
|t::q -> (t=x) || mem x q;;

let rec mem_fst x l = match l with
| [] -> false
| t::q  ->  (fst t = x) || mem_fst x q ;; 

let rec assoc x l = match l with
| [] -> failwith "Not_found"
| t::q when (fst t)=x -> snd t
| t::q -> assoc x q;;





(* question 3 *)

let reconnu a mot =
  let rec aux e l= match l with
    | []   -> mem e a.accept
    | t::q when (mem_fst (e,t) a.delta) -> aux (assoc (e, t) a.delta) q
    | _ -> false
  in aux a.init mot ;;


(* question 4 *)

let genere_fort d =
  let rec aux = function
    | q when q = d -> []
    | q -> [(q, 0), (2*q) mod d; (q, 1), (2*q+1) mod d] @ (aux (q+1))
  in {init = 0; accept = [0]; delta = aux 0} ;;


(* test *)
let a3 = genere_fort 3 in reconnu a3 (binaire_fort 365) ;;
let a3 = genere_fort 3 in reconnu a3 (binaire_fort 364) ;;
let a3 = genere_fort 3 in reconnu a3 (binaire_fort 363) ;;
let a3 = genere_fort 3 in reconnu a3 (binaire_fort 362) ;;

let a5 = genere_fort 5 in reconnu a5 (binaire_fort 365) ;;
let a5 = genere_fort 5 in reconnu a5 (binaire_fort 364) ;;
let a5 = genere_fort 5 in reconnu a5 (binaire_fort 363) ;;
let a5 = genere_fort 5 in reconnu a5 (binaire_fort 362) ;;
let a5 = genere_fort 5 in reconnu a5 (binaire_fort 361) ;;
let a5 = genere_fort 5 in reconnu a5 (binaire_fort 360) ;;

(* AUTOMATES NON DETERMINISTES *)

type ('a, 'b) afnd = {nd_init: 'a list ;
                      nd_accept: 'a list ;
                      nd_delta: (('a * 'b) * 'a) list} ;;


(* question 5 *)

(* Idee : echanger etats initiaux et finals, et echanger le sens des fleches *)
let genere_faible d =
  let a = genere_fort d in
  let rec aux = function
    | [] -> []
    | ((q, a), r)::v -> ((r, a), q)::(aux v)
  in {nd_init = a.accept; nd_accept = [a.init]; nd_delta = aux a.delta} ;;


(* question 6 *)
let rec exists pred l = match l with
| [] -> false
| t::q -> (pred t) || exists pred q;;


(* question 7 *)

let reconnu2 a mot =
  let rec aux e m delta = match (m, delta) with
    | ([],_) -> mem e a.nd_accept (* le mot vide est-il reconnu : etat e = etat acceptant ? *)
    | (_,[]) -> false (* transitions vides *)
    (* le mot commence par a1 et la transition est (e,a1,r), alors soit on prend la transition et on va en r pour lire m1,
    soit on ne prend pas la transition et on ne bouge pas *)
    | (a1::m1, ((q, c), r)::v) when q = e && c = a1 -> aux r m1 a.nd_delta || aux e m v
    
(* si la transition ne correspond pas a ((e,a1),_), on parcourt le reste des transitions *)
    | (m, _::v) -> aux e m v
(* on teste chaque etat initial : le mot est reconnu si l'un des etats fournit un chemin acceptant *) 
  in exists (function e -> aux e mot a.nd_delta) a.nd_init ;;

(* test *)
let af5 = genere_faible 5 in reconnu2 af5 (binaire_faible 3664064) ;;
let af5 = genere_faible 5 in reconnu2 af5 (binaire_faible 3664061) ;;
let af5 = genere_faible 5 in reconnu2 af5 (binaire_faible 3664063) ;;
let af5 = genere_faible 5 in reconnu2 af5 (binaire_faible 3664065) ;;



















