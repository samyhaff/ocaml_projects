type arbre = Vide | Noeud of arbre * arbre;;

let rec genereComplet n = match n with
  |1 -> Arbre(Vide, Vide)
  |_ -> let u = genereComplet (n - 1) in Noeud(u, u);;

let rec hauteur arbre = match arbre with
  |Vide -> 0
  |Noeud(g, d) -> 1 + max (hauteur g) (hauteur d);;

let rec compterFeuilles arbre = match arbre with
  |Vide -> 0
  |Noeud(Vide, Vide) -> 1
  |Noeud(g, d) -> compterFeuilles g + compterFeuilles d;;

let testComplet arbre =
  let n = compterFeuilles arbre in
  let h = hauteur arbre in
  int_of_float(2. ** (float_of_int h)) = n;;

let profondeurMin a = match a with
  |Vide -> 0
  |Noeud(g, d) -> 1 + min (profondeurMin g) (profondeurMin d);;

type 'a arbre = Vide | Feuille of 'a | Noeud of 'a * 'a arbre * 'a arbre;;

let nFeuilles a = match a with
  |Vide -> 0
  |Feuille(_) -> 1
  |Noeud(_, g, d) -> (nFeuilles g) + (nFeuilles d);;

(* le cheminement est la somme des profondeurs des noeuds d'un arbre *)
let cheminement a =
  let rec cheminementAux a aux =
    |Vide -> 0
    |Feuille(a) -> aux
    |Noeud(_, g, d) -> aux + cheminementAux g (aux +1) + cheminementAux d (aux + 1)
  in cheminementAux a 0;;
