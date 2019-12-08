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
