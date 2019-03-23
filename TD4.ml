open Array;;
open List;;

(* selection sort listes *)

let rec selectMin liste = match liste with
  |[] -> failwith "liste vide"
  |t::[] -> t, []
  |t::q -> let u, v = selectMin q in
    if u < t then u, t::v else t, q;;

let rec triSelection liste = match liste with
  |[] -> []
  |t::[] -> [t]
  |t::q -> let u, v = selectMin q in
    if u < t then u::triSelection(t::v) else t::triSelection(q);;

(* insertion sort listes *)

let rec insere x liste = match liste with
  |[] -> [x]
  |t::_ when x >= t -> t::(x::q)
  |t::q -> insere x q;;

let rec triInsertion liste = match liste with
  |[] -> []
  |t::q -> insere t (triInsertion q);;

(* insertion sort array *)

let rec echange t i j =
  let u = t.(j) in t.(j) <- t.(i); t.(i) <- u;;

let rec insere t i =
  if i = 0 then ()
  else if t.(i) < t.(i - 1) then
    begin
      echange t i (i - 1);
      insere t (i - 1);
    end;;

let triInsertion t =
  let n = Array.length t in
  let rec triInsertionAux t i =
    if i = n then ()
    else
      begin
        insere t i;
        triInsertionAux t (i - 1);
      end;
  in triInsertionAux t 0;;

(* selection sort Arrays *)

let rec minAux t i =
  let n = Array.length t in
  if i = n - 1 then i
  else
    begin
      let u = minAux t (i - 1) in
      if t.(u) < t.(i) then u else i;
    end;;

let triSelection t =
  let n = Array.length t in
  let rec triSelectionAux t i =
    if i < 1 then ()
    else
      begin
        let ind = minAux t i in
        echange t i ind;
        triSelectionAux t (i - 1);
      end;
  in triSelectionAux t 0;;

(* exercice 1 *)

let reverse t =
  let n = Array.length t - 1 in
  let rec reverseAux t c =
    if c > n / 2 then t
    else
      begin
        echange t c (n - c);
        reverseAux t (c + 1);
      end;
  in reverseAux t 0;;

(* exercice 2 *)

(* 'a Array -> int -> int -> 'a Array *)

let t = [|1;2;3|];;

let mystere tab =
  let n = Array.length tab in
  for i = 0 to n - 1 do
    echange tab i (n - 1 - i);
  done;;

mystere t;;

(* exercice 3 *)

let dichotomique t x =
  let n = Array.length t - 1 in
  let rec dichotomiqueAux t x l r =
    let m = l + ()(l - r) / 2) in
    if t.(m) = x then m
    else if t.(m) > x then dichotomiqueAux t x l m
    else dichotomiqueAux t x m r
in dichotomiqueAux t x 0 n;;

(* arbres binaires *)

(* un abre binaire est soit le graphe vide soit un sous arbre suivi d'un noeud et d'un sous arbre *)

(* un arbre binaire strict est soit une feuille soit un noeud encadré par 2 arbres binaires stricts *)

type 'a arbre =
  (* aucun noeud privilegié par rapport aux autres *)
  |Vide
  |Noeud of 'a * 'a arbre * 'a arbre;;

type ('a, 'b) arbre =
  (* etiquette des noeuds différente de celle des feuilles *)
  |Feuille of 'a
  |NoeudInterne of 'b * ('a, 'b) arbre * ('a, 'b) arbre;;

let a = Noeud('+', Noeaud('*', Feuille(3), Feuille(4)), Feuille(5))

type 'a arbre =
  (* seules les feuilles portnent une etiquette *)
  |Feuille of 'a
  |NoeudInterne of 'a arbre * 'a arbre;;

let rec nombreFeuilles arbre = match arbre with
  |Feuille(a) -> 1
  |NoeudInterne(a, b) -> nombreFeuilles a + nombreFeuilles b;;

let rec profondeur arbre = match arbre with
  |Feuille(a) -> 0
  |NoeudInterne(a, b) -> 1 + max (profondeur a) (profondeur b);;
