type 'a aqc = {
  mutable n : int;
  mutable donnes : 'a array
};;

let filsGauche p = 2 * p + 1;;
let filsGauche p = 2 * p + 2;;

let valeurFilsGauche abre m =
  if 2 * m + 1 >= arbre.n then failwith "le noeud fourni en entrée est une feuille"
  else arbre.donnees.(2 * m + 1);;

let estRacine n = (n = 0);;

let echange t m1 m2 =
  let u = t.donnees in
  let x1 = u.(m1) and x2 = u.(m2) in
  u.(m1) <- x2;
  u.(m2) <- x1;;

let maximum t = t.donnees.(0);;

let estVideSousArbre p arbre = (p >= arbre.n);;

let rec percoleDescendant t p =
  let fg = filsGauche p in
  let fd = filsDroit p in
  let m = if not (estVideSousArbre fg t) && t.donnees.(fg) > t.donnees(p) then fg else p in
  let m' = if not (estVideSousArbre fd t) && t.donnees.(fd) > t.donnees(p) then fd else m in
  if m' <> 0 then begin
    echange t m' 0;
    percoleDescendant t m'
  end;;
