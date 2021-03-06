open Stack;;
open Array;;
open List;;
open String;;

(* tri fusion *)

let rec fusion l1 l2 = match l with
  |[], l2 -> l2
  |l1, [] -> l1
  |t1::q1, t2::q2 ->
    if t1 < t2 then t1::(fusion q1 l2)
    else t2::(fusion l1 q2);;

let rec split l = match l with
  |[] -> [], []
  |t1::[] -> [t1], []
  |t::q -> let u, v = split q in t::v, u

let rec triFusion l = match l with
  |[] -> []
  |t1::[] -> [t1]
  |l -> let l1, l2 = split l in fusion (triFusion l1) (triFusion l2);;

(* tri par sélection:  *)

let rec selectMin liste = match liste with
  (* retourne le min et le reste de la liste
  — On parcourt une première fois la liste/le tableau à trier de longueur n,
— On trouve l’élément de valeur minimale,
— On l’échange avec l’élément en première position,
— On recommence avec les n − 1 qui suivent et on cherche le plus petit parmi ceux là, on le
met en deuxième position.
— On recommence le procédé.*)
  |[] -> failwtih "liste vide"
  |t::[] -> l, []
  |t::q -> let u, v = selectMin q in if u < t then u, t::v else t, q;;

let rec triSelection l = match l with
  |[] -> []
  |t::[] -> [t]
  |t::q -> let u,v = selectMin q in
    if u < t then u::triSelection t::v
    else t::(triSelection q);;

(* tri par insertion
— On parcourt la liste ou le tableau à trier de droite à gauche.
— Lorsque l’on considère l’élément d’indice k, on suppose que les k − 1 éléments à sa gauche
sont déjà triés.
— On fait remonter l’élément d’indice k à sa place dans le sous-tableau/la sous-liste trié.e par
des échanges successifs entre deux éléments consécutifs.
Commençons par rappeler l’implémentation récursive de l’algorithme que nous avons faites
   avec des listes en TP *)

let rec insere x liste =
  match liste with
  |[] -> [x]
  |t::_ when x <= t -> x::liste
  |t::q -> t::(insere x q);;

let rec triInsertion l = match l with
  |[] -> []
  |t::q -> insere t (triInsertion q);;

(* PILES et FILES *)

(* — un constructeur de pile ex nihilo : val create : unit -> 'a t,
— une fonction d’empilement au sommet de la pile : val push : 'a -> 'a t -> unit,
— une fonction de dépilement en sommet de pile : val pop : 'a t -> 'a,
— une fonction permettant de tester si la pile est vide : val is_empty : 'a t -> bool,
— une fonction pour accéder au sommet de la pile : val top : 'a t -> 'a. *)

(* postfixe *)

let operations a b e = match e with
  |"+" -> a + b
  |"x" -> a * b
  |"-" -> a - b
  |"/" -> a / b

let postfixe chaine =
  let operateurs = "+-x/" in
  let n = String.length chaine - 1 in
  let s = Stack.create() in
  for i = 0 to n
  do
    let v = (String.sub chaine i 1) in
    if membre v operateurs (* vérifie si caractère est bien un opérateur *)
    then
      begin
        let a = int_of_string(pop s) in
        let b = int_of_string(pop s) in
        let c = operations a b v in
        push (string_of_int(c)) s;;
      end
    else
      push v s
  done;
  pop s;;

(*— un constructeur : val create : unit -> 'a t,
— une fonction d’enfilement : val add : 'a -> 'a t -> unit
— une fonction de défilement : val take : 'a t -> 'a
— une fonction de test de la vacuité : val is_empty : 'a t -> bool. *)

(* construction du type pile *)

type 'a pile = ('a list) ref;;

let empile s x =
  s := x::!s

let depile s = match !s with
  |[] -> failwith "liste vide"
  |t::q -> s := q;
  t;;

let estVide s = match !s with
  |[] -> True
  |_ -> False

(* création dy type file *)

type 'a file = {mutable tete : 'a list ; mutable queue : 'a list};;

let estVide file = file.tete = [] && file.queue = []

let defile f = match f.tete f.queue with
  |[], [] -> failwith "file vide"
  |[], _ ->
    t.tete <- List.rev f.queue;
    f.tete <- tl f.tete;
    hd f.tete
  |t::q -> f.tete <- q; t;;

let enfile f x = f.queue = x x::(f.queue);;

(* arbres *)

(*— Le parcours préfixe : on visite le noeud lorsque l’on descend dans la branche, et qu’on le rencontre pour la première fois,
— Le parcours postfixe (ou suffixe) : on visite le noeud lorsque l’on remonte de la branche et qu’on le rencontre pour la dernière fois,
  — Le parcours infixe : on visite chaque noeud ayant un fils gauche la seconde fois qu’on le rencontre et chaque noeud sans fils gauche à la descente (l’idée est de parcourir le noeud entre la fin de parcours du sous arbre gauche de ses descendants et le sous arbre droit) . *)

(*— Parcours préfixe : on visite le sous arbre gauche de la racine, puis le sous arbre droit puis la racine,
— Parcours postfixe : Parcours préfixe : on visite la racine puis son sous arbre gauche de la racine, puis le sous arbre droit,
  — Parcours infixe : on visite le sous arbre gauche de la racine, puis la racine puis le sous arbre droit, *)

type 'a arbre = Vide | Noeud of 'a * 'a arbre * 'a arbre;;

let rec parcoursPrefixe arbre = match arbre with
  |Vide -> []
  |Noeud(a, g, d) -> [a] @ (parcoursPrefixe g) @ (parcoursPrefixe d);;

let rec parcoursPostfixe arbre = match arbre with
  |Vide -> []
  |Noeud(a, g, d) -> (parcoursPostfixe g) @ (parcoursPostfixe d) @ [a];;

let rec parcoursInfixe arbre = match arbre with
  |Vide -> []
  |Noeud(a, g, d) -> (parcoursInfixe g) @ [a] @ (parcoursInfixe d);;

(* dictionaires *)

type clef = Rien | Int of int;;
type 'a donnee = Vide | Donnee of 'a;;
type 'a objet = clef * ('a donnee);;

let insere tab objet = match objet with
  |Int(c), data -> tab.(c) <- (c, data)
  |(_, _) -> ();;

let suppresion tab indice =
  tab.(indice) <- (Rien, Vide);;

let recherche tab clef =
  tab.(clef);;

let creerTable =
  Array.make n (Rien, Vide);;

(* — création d’un dictionnaire vide ;
— test d’égalité au vide ;
— test de présence d’un élément ayant une clé k donnée ;
— retrait de l’élément ayant une clé k donnée (s’il existe) ;
   — ajout d’un élément e avec une clé k donnée (s’il n’y a pas déjà d’élément de clé k). *)
