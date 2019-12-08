(* RECONSTRUCTION de l'ARBRE à partir d'un PARCOURS PREFIXE *)

type ('n, 'f) arbre = Feuille of 'f | Noeud of 'n * (('n, 'f) arbre) * (('n, 'f) arbre);;

type ('n, 'f) noeud = N of 'n | F of 'f ;;


let list_pref_1 = [N("+"); F(2); N("+"); N("*") ; F(5); F(3); F(4)];;


let reconstruction_prefixe l = 
	let rec aux ll = match ll with
		| [] 				-> failwith "prefixe incorrect"
		| (F f)::reste 	-> Feuille f , reste
		| (N n):: reste 	-> 		let g,r1  = aux reste in
									let d,r2 = aux r1 in
									Noeud(n,g,d), r2
	in match (aux l) with
		| a, [] 	-> a
		| _ 		-> failwith "parcours prefixe incorrect" ;;



reconstruction_prefixe list_pref_1;;




(* EVALUATION d'une EXPRESSION ARITHMETIQUE a partir d'un parcours SUFFIXE *)

(* on utilise une pile : 
si on lit une feuille, on empile
si on lit un noeud, on sort deux éléments de la pile, on évalue avec le noeud binaire (opération), et on rempile le résultat
*)
(* On suppose ici que les seuls connecteurs binaires présents sont "+" et "*". *)

let list_suffixe_1 = [F(2) ; F(5); F(3) ; N("*") ; F(4) ; N("+") ; N("+")];;



let evaluation_suffixe l = 
	let rec aux pile parcours = match (pile, parcours) with
		| p, F(f)::reste 			 -> aux (f::p) reste
		| d::g::p , N("+")::reste -> aux ((g+d)::p) reste
		| d::g::p , N("*")::reste -> aux ((g*d)::p) reste
		| d::g::p , N (_)::q -> failwith "operation inconnue"
		| [res] , [] -> res
		| _ 		-> failwith "suffixe incorrect" 
	in aux [] l ;;
		

evaluation_suffixe list_suffixe_1;;

(* avantage : la pile ne contient en cours d'execution que des entiers : chaque connecteur binaire est immédiatement évalué *)


(* EXERCICE ARBRE 5 *) 
(* Question 2 *)

(* RECONSTRUCTION de l'ARBRE à partir d'un PARCOURS SUFFIXE *)

type ('n, 'f) arbre = Feuille of 'f | Noeud of 'n * (('n, 'f) arbre) * (('n, 'f) arbre);;

type ('n, 'f) noeud = N of 'n | F of 'f ;;

let reconstruction_suffixe l =
	let rec aux pile parcours = match (pile, parcours) with
		| p, F(f)::reste 			-> aux (Feuille(f)::p) reste
		| d::g::p, (N n)::reste  -> aux (Noeud(n,g,d)::p) reste
		| [arbre], [] -> arbre
		|_ -> failwith "erreur syntaxe suffixe" 
	in aux [] l;;

let lis_suffixe = [ F(2); F(5);F(3);   N("*") ;  F(4) ; N("+") ;N("+") ];;

reconstruction_suffixe lis_suffixe;;


(* Question 3 *)

(* EVALUATION d'une EXPRESSION ARITHMETIQUE a partir d'un parcours PREFIXE *)


let lis_prefixe = [ N("+") ; F(2); N("+") ; N("*") ;F(5);F(3);    F(4)  ];;


(* si le parcours donné est préfixe, il va commencer par des noeuds internes : ces noeuds internes vont être stockés dans une pile et on effectuera l'évaluation dès que deux feuilles se présenteront.
On suppose ici que les seuls connecteurs binaires présents sont "+" et "*".
*)

let evaluation_prefixe l = 
  let rec aux pile parcours = match (pile, parcours) with
    | p, (N n)::reste -> aux (N(n)::p) reste
    | F(f2)::F(f1)::N("+")::p, reste -> aux (F(f1+f2)::p) reste
    | F(f2)::F(f1)::N("*")::p, reste -> aux (F(f1*f2)::p) reste
    | [res], [] -> res
    | p, F(f)::reste -> aux (F(f)::p) reste 
    | _ -> failwith "erreur syntaxe prefixe"
  in match (aux [] l) with
  	|F r -> r
  	| _ -> failwith "syntaxe prefixe incorrect" ;;

evaluation_prefixe lis_prefixe;;

(* inconvenient : la pile contient en cours d'evaluation des Feuilles et des Noeuds internes *)






