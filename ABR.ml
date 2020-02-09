type 'a arbre = Vide | Noeud of 'a * 'a arbre * 'a arbre

let rec insertion a x = match a with
  |Vide -> Noeud(x, Vide, Vide)
  |Noeud(y, g, d) when y = x -> a
  |Noeud(y, g, d) when x < y -> Noeud(y, insertion g x, d)
  |Noeud(y, g, d) -> Noeud(y, g, insertion d x);;

let rec creation l = match l with
  |[] -> Vide
  |t::q -> insertion (creation q) t;;

let rec partition a x = match a with
  |Vide -> (Vide, Vide)
  |Noeud(y, g, d) -> when y = x -> (g, d)
  |Noeud(y, g, d) when x < y -> let (g2, d2) = partition g in (g2, Noeud(y, d2, d))
  |Noeud(y, g, d) -> let (g2, d2) = partition d in (Noeud(y, g2, g), d2);;

let insertion_racine a x = let (g, d) = partition a x in Noeud(x, g, d);;

let rec recherche a x = match a with
  |Vide -> false
  |Noeud(r, g, d) when r = x -> true
  |Noeud(r, g, d) when x < r -> recherche g x
  |Noeud(r, g, d) -> recherche d x;;

let minimun a = match a with
  |Vide -> failwith "pas de minimum car l'arbre est vide..."
  |Noeud(x, vide, _) -> x
  |Noeud(x, g, _) -> minimum g;;

let maximum a = match a with
|Vide -> failwith "pas de maximum car l'arbre est vide..."
|Noeud(x, _, vide) -> x
|Noeud(x, _, d) -> maximum d;;

let rec fusion g d = match g, d with
  |Vide, _ -> d
  |_, Vide -> g
  |(r1, g1, d1), (r2, g2, d2) -> Noeud(r1, g1, N(r2, fusion g2 d1), d2);;

let rec supression a x = match a with
  |Vide -> failwith "rien à supprimer, l'arbre est déjà vide..."
  |Noeud(y, g, d) when y = x -> fusion g d
  |Noeud(y, g, d) when y < x -> (y, g, supression d x)
  |Noeud(y, g, d) -> (y, supression g x, d);;

let rec parcours_prefixe arbre = match arbre with
  |Nil -> []
  |Noeud(r, g, d) -> r :: parcours_prefixe g :: parcours_prefixe d;;

let rec parcous_suffixe arbre = match arbre with
  |Nil -> []
  |Noeud(r, g, d) -> parcours_suffixe g @ parcours_suffixe d @ [r];;

let rec parcours_infixe arbre = match arbre with
  |Nil -> []
  |Noeud(r, g, d) -> parcours_infixe g @ [r] @ parcours_infixe;;

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

let rec liste_profondeur arbre k = match (arbre, k) with
  |(Nil, _) -> []
  |(Noeud(x, g, d), 0) -> [x]
  |(Noeud(x, g, d), k) -> (liste_profondeur g (k - 1)) @ (liste_profondeur d (k - 1));;

let rec appartient a x = match a with
  |Nil -> false
  |Noeud(r, g, d) -> r = x || appartient g x || appartient d x

let rec map_ab f a = match a with
  |Nil -> Nil
  |Noeud(r, g, d) -> Noeud(f r, map_ab g, map_ab d);;
