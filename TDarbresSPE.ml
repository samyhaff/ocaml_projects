type arbre = Nil | Noeud of int * arbre * arbre;;

let rec liste_profondeur arbre k = match (arbre, k) with
  |(Nil, _) -> []
  |(Noeud(x, g, d), 0) -> [x]
  |(Noeud(x, g, d), k) -> (liste_profondeur g (k - 1)) @ (liste_profondeur d (k - 1));;

let rec hauteur a = match arbre with
  |Nil -> 0
  |Noeud(_, g, d) -> 1 + max (hauteur g) (hauteur d);;

let rec est_complet a = match a with
  |Nil -> true
  |Noeud(x, g, d) -> est_complet g && est_complet d && (hauteur g = hauteur d);;

let complet_bis a =
  let rec complet_aux a = match a with
  	|Nil -> true,0
  	|Noeud(r,g,d) ->
  	let (b1, h1) = complet_aux g
  	and (b2,h2) = complet_aux d in
  	(b1 && b2 && h1=h2, 1+ max h1 h2)
  in let (b,_) = complet_aux a  in b;;

let est_avl a =
  let rec aux a = match a with
    |Nil -> true, 0
    |Noeud(r, g, d) ->
      let (t1, h1) = aux g in
      let (t2, h2) = aux d in
      (t1 && t2 && abs (h1 - h2) <= 1, 1 + max h1 h2)
  in (t, _) = aux a in t;;

let rec appartient a x = match a with
  |Nil -> false
  |Noeud(r, g, d) -> r = x || appartient g x || appartient d x

let rec map_ab f a = match a with
  |Nil -> Nil
  |Noeud(r, g, d) -> Noeud(f r, map_ab g, map_ab d);;

let rec mirroir a b = match a, b with
  |Nil, Nil -> true
  |Noeud(r1, g1, d1), Noeud(r2, g2, d2) -> r1 = r2 && mirroir g1 d2 && mirroir d1 g2;;

let symetric a = match a with
  |Nil -> true
  |Noeud(_, g, d) -> mirroir g d;;

type arbre = Nil | Noeud of arbre * arbre;;

let rec genere_complet n = match n with
  |0 -> Nil
  |n -> let a = genere_complet (n - 1) in Noeud(a, a);;

type 'a arbre = Feuille of 'a | Noeud of 'a * 'a arbre * 'a arbre | Nil;;

let chemin a x =
  let rec aux a l = match a with
    |Feuille(_) -> []
    |Noeud(r, g, d) when r = x -> l
    |Noeud(r, g, d) -> (aux g (r::l)) @ (aux d (r::l))
  in aux a [];;
