(* TD ARBRES *)

(* Exercice 1 : Noeud � profondeur k *)

type arbre = Nil | Noeud of int * arbre * arbre ;;


(* premi�re version *)
let rec profondeur a k = match (a,k) with
| (Nil,_) -> []
| (Noeud(x,g,d),0) -> [x]
| (Noeud(x,g,d),k) -> 
(profondeur g (k-1)) @ (profondeur d (k-1));;

(* attention au surcout engendr� par la concat�nation.
 *)

(* Analyse du co�t dans le pire cas : 
Soit a un arbre complet de hauteur h o� l'on recherche la profondeur h.
Le nombre d'insertions C(h) effectu�es est donn� par la relation
C(h) = 2 C(h-1) + 2^(h-1) si l'on cherche � la profondeur h.


On se ram�ne � une suite arithm�tico-g�om�trique en divisant par 2^h :
En notant D(h) = C(h) / 2^h, on obtient
D(h) = D(h-1) + 1/2
Ainsi, en it�rant (ou t�lescopant) on trouve

C(h) =O(2^h * 1/2 h) =  O(h 2^(h-1)).

Si l'arbre a est complet, alors sa taille (nombre de noeuds) est 2^(h+1) - 1. En notant n cette taille, 
on trouve une complexit� en O( n log(n) )  
ce qui n'est donc pas optimal (� cause de la concat�nation @).

On pourrait esp�rer une complexit� lin�aire en la taille de l'arbre.
*)



(* version am�lior�e avec un seul parcours de l'arbre *)
let profondeurbis a k = 
let rec prof a k acc = match (a,k) with
| (Nil,_) -> acc
| (Noeud(x,g,d),0) -> x :: acc
| (Noeud(x,g,d),k) -> let acc1 = prof g (k-1) acc in prof d (k-1) acc1
in prof a k [];;


(* Chaque �tiquette n'est alors ins�r�e qu'une seule fois en t�te de liste, ce qui est optimal.
Pour l'arbre binaire complet de hauteur h et de taille n = 2^(h+1) - 1, on trouve une complexit� en
O( 2^h ) = O(n), donc lin�aire en la taille de l'arbre.
*)

(* g�n�ration automatique d'un arbre complet de hauteur n, pour les exemples *)

let rec gentree n = match n with
| 0 -> Nil
| n -> Noeud( n-1, gentree (n-1), gentree (n-1));; 

let a = gentree 5;;

#trace profondeur;;
profondeur a 4;; 
#untrace profondeur ;;

#trace profondeurbis;;
profondeurbis a 4;;
#untrace profondeurbis ;;


(* Exercice 2 : Arbre complet *)

(* Le calcul de la hauteur est de complexit� lin�aire en la taille de l'arbre *)

let rec hauteur a = match a with
| Nil -> 0
| Noeud(_,g,d) -> 1+ max (hauteur g) (hauteur d);;


let rec est_complet a  = match a with
| Nil -> true
| Noeud (r,g,d) -> est_complet g && est_complet d && hauteur d = hauteur d ;;



(*
Si a est l'arbre binaire complet de taille n, on obtient, en notant C(n) le cout de la fonction est_complet : 

C(n) = 2 * C(n/2) + lambda * n

Les deux appels r�cursifs se font sur les sous-arbres de taille n/2 (partie enti�re inf�rieure / sup�rieure)
et le co�t de hauteur est lin�aire en n.
 
 Le th. Diviser-Pour-R�gner (Master Theorem)
 donne une complexit� : C(n) = O (n log(n) )

Le surco�t est celui de hauteur ici.
 *)
 


(* Si l'arbre n'est pas complet, les calculs asymptotiques sont d�licats � cause de l'�valuation paresseuse du &&.
Mais une majoration donne 

C(n) <= C(p) + C(q) + O(n)
o� p+q = n-1
On prouve alors que dans le pire cas
(p=0 et q=n-1 � chaque �tape par exemple) 
 C(n) = O(n^2)
*)


#trace est_complet;;
est_complet a;;
#untrace est_complet;;

(* version optimis�e  : Lin�aire *)
(* 
La fonction auxiliaire renvoie un couple form� d'un bool�en (true si l'arbre est complet) et de la hauteur.
*)

let complet_bis a =

let rec complet_aux a = match a with
	| Nil -> true,0
	| Noeud(r,g,d) -> 
	let (b1, h1) = complet_aux g 
	and (b2,h2) = complet_aux d in
	(b1 && b2 && h1=h2, 1+ max h1 h2)

in 
let (b,_) = complet_aux a  in b;;


(* Complexit� : 
Si a est vide, C(0) = 1.
Si a est de taille n>0, et g de taille p et d de taille q, on a  : p+q = n-1
C(n) = C(p) + C(q) + O(1)
On prouve sans difficult� que C(n) = O(n)
*)


#trace complet_aux_bis;;
complet_aux_bis a;;
#untrace complet_aux_bis;;


(* Exercice 3 : Arbre equilibre *)


(*  Question 1
Par r�currence sur la hauteur de l'arbre,
on montre que
"Tout AVL de hauteur h poss�de au moins F(h) noeuds"

init : h = 0, alors a=Nil, et F(0)=0.

hered : si a est hauteur h>0, on suppose la propri�t� pour les arbres de hauteur strictement inf�rieure � h.
Soit a = (r,g,d). L'un des deux sous-arbres est de hauteur (h-1) donc contient au moins F(h-1) noeuds.
L'autre est au moins de hauteur h-2 (pour pr�server l'�quilibre AVL), il contient donc au moins F(h-2) noeuds.
Ainsi, a contient au moins 1+ F(h-1) + F(h-2) = 1 + F(h) noeuds, ce qui conclut la r�currence.
*)


(* Question 2 *)
(*
Comme F(h) = O (phi^h) o� phi = (1 + sqrt(5)) /2, 
en notant n la taille de l'arbre a de hauteur h, on a 
n >= F(h), donc n >= K phi^h
donc log n >= h
(en prenant le log � base phi)
Ainsi, on trouve que la hauteur h = O (log n)
  *)



(* meme d�marche que pour le test est_complet:
on calcule dans la fonction auxiliaire un couple (bool,int) contenant true si l'arbre est un AVL et le d�s�quilibre h(g) - h(d).
Ainsi, en un seul parcours de l'arbre (donc en complexit� lin�aire en la taille de l'arbre), on obtient le r�sultat du test
 *)

let avl a =
	let rec avl_aux a = match a with
		| Nil -> true,0
		| Noeud(r,g,d) -> 
		let (b1, h1) = avl_aux g 
		and (b2,h2) = avl_aux d in
		(b1 && b2 &&  abs(h1-h2)<=1, 1+ max h1 h2)
	in let (b,_) = avl_aux a  in b;;

#trace avl;;
avl a;;
#untrace avl;;

(* Exercice 4 : Fonctions sur les arbres *)

(* quelques fonctions sur les arbres *)

let rec appartient x a = match a with
| Nil -> false
| Noeud(r,g,d) -> r=x || appartient x g || appartient x d ;;


let rec map_ab f a = match a with
| Nil -> Nil
| Noeud(r,g,d) -> Noeud (f r, map_ab f g, map_ab f d);;


let rec miroir a1 a2 = match (a1, a2) with
| (Nil, Nil) -> true
| (Nil, _) | (_, Nil)-> false
| (Noeud(r1,g1,d1), Noeud(r2, g2, d2)) -> 
	r1=r2 && (miroir g1 d2) && (miroir g2 d1);;


let symetric a = match a with
| Nil -> true
| Noeud(r,g,d) -> miroir g d;;









