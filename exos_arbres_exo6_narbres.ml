(* Exercice 6 *)

type 'a n_arbre = Vide | N of 'a * ('a n_arbre list);;


(* Test *)

let a1 = N ( 4,  [ N (2, [ N(1, []) ; N(1,[])]) ; N (3, [ N(1, []) ; N(1,[])]) ; N (5, [ N(1, []) ; N(1,[])])  ]   );;

(* Preliminaire *)

(* Generation d'un arbre n-aire de hauteur h de taille arbitraire
les cles des noeuds sont entre 0 et 9 et l'arite de chaque noeud egalement *)

let rec gen_arb h = match h with
(* on genere une feuille *)
| 0 -> let cle = Random.int 10 in N(cle, [])
(* on genere une racine, un nb de fils et une foret de hauteur h-1 *)
| h -> let nb_fils = Random.int 10 and cle = Random.int 10 in
N ( cle, gen_foret (h-1) nb_fils)

(* k est la hauteur des arbres de la foret et n est le nombre d'arbres *)
and gen_foret k n = match n with
| 0 -> []
| _ -> (gen_arb k)::(gen_foret k (n-1))
;;

let exple = gen_arb 5;;

(* ****** *)
(* TAILLE *)
(* ****** *)

(* Avec List.fold_left et List.map *)
let rec taille a = match a with
| Vide -> 0
| N (r,l) -> List.fold_left (+) 1 (List.map taille l);;


(* Avec recursivite croisee *)
let rec taille_bis a = match a with
| Vide -> 0
| N(r,l) -> 1 + taille_foret l

and taille_foret b = match b with
| [] -> 0
| t::q -> (taille_bis t) + taille_foret q;;


(* En recursivite simple, mais avec deux appels recursifs *)
let rec taille_ter a = match a with
| Vide -> 0
| N(r,[]) -> 1
| N(r,t::q) -> (taille_ter t) + taille_ter (N (r,q)) ;;




taille a1,
taille_bis a1,
taille_ter a1;;

taille exple,
taille_bis exple,
taille_ter exple;;






(* ******* *)
(* HAUTEUR *)
(* ******* *)


(* Avec List.fold_left et List.map *)

let rec hauteur a = match a with
| Vide -> -1
| N (r,l) -> 1+ List.fold_left max (-1) (List.map hauteur l);;


(* Avec recursivite croisee *)

let rec haut_arb a = match a with
| Vide -> -1 
| N(r,l) -> 1 + haut_foret l

and haut_foret b = match b with
| [] -> (-1)
| t::q -> max (haut_arb t) (haut_foret q) ;;


(* Avec recursivite simple mais deux appels *)

let rec haut_ter a = match a with
| Vide -> -1
| N(r,[]) -> 0
| N(r,t::q) -> max ( 1 + haut_ter t) (haut_ter (N(r,q)));;

hauteur a1,
haut_arb a1,
haut_ter a1;;


hauteur exple,
haut_arb exple,
haut_ter exple;;


(* ******* *)
(*  SOMME  *)
(* ******* *)

(* Avec List.fold_left et List.map *)
let rec sum a = match a with
| Vide -> 0
| N (r,l) -> r + List.fold_left (+)  0 (List.map sum l);;



(* Avec recursivite croisee *)
let rec sum_arb a = match a with
| Vide -> 0
| N(r,l) -> r + sum_foret l
and sum_foret b = match b with
| [] -> 0
| t::q -> (sum_arb t) + (sum_foret q);;


(* Avec recursivite simple mais deux appels *)
let rec sum_ter a = match a with
| Vide -> 0
| N(r, []) -> r
| N(r, t::q) -> (sum_ter t) + sum_ter (N(r,q));;


sum a1,
sum_arb a1,
sum_ter a1
;;

sum exple,
sum_arb exple,
sum_ter exple
;;


(* ************ *)
(*  APPARTIENT  *)
(* ************ *)


(* Avec List.fold_left et List.map *)

let rec appartient x a = match a with
| Vide -> false
| N (r,l) -> 
x=r || List.fold_left (||) false (List.map (appartient x) l);;


(* Avec List.exists *)
let rec appartient_bis x a = match a with
| Vide -> false
| N(r,l) -> x=r || List.exists (appartient_bis x) l;;


(* Avec recursivite croisee *)

let rec app_arb x a = match a with
| Vide -> false
| N(r,l) -> r=x || app_foret x l

and app_foret y b  = match b with
| [] -> false
| t::q -> (app_arb y t) || (app_foret y q);;


(* Avec recursivite simple mais deux appels *)
let rec app_ter x a = match a with
| Vide -> false
| N(r, []) -> r=x
| N (r, t::q) -> (app_ter x t) || (app_ter x (N(r,q)));;


(* Tests *)
appartient 5 a1, appartient_bis 5 a1, app_arb 5 a1, app_ter 5 a1;;
appartient 0 a1, appartient_bis 0 a1, app_arb 0 a1, app_ter 0 a1;;

appartient 5 exple, appartient_bis 5 exple, app_arb 5 exple, app_ter 5 exple;;
appartient 10 exple, appartient_bis 10 exple, app_arb 10 exple, app_ter 10 exple;;











