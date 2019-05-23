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
    let m = l + (l - r) / 2) in
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

(* exercice 4 *)

type arbre = Vide | Noeud of arbre * arbre;;

let rec genereComplet n =
  if n = 0 then Noeud(Vide, Vide)
  else Noeud(genereComplet n - 1, genereComplet n - 1);;

(* test complet: utiliser la formule du cours *)

type ('f, 'n) arbre =
  |Feuille of 'f
  |NoeudInterne of ('f, 'n) arbre * 'n * ('f, 'n) arbre;;

(* différence entre les 2 types: le 2e permet aux feuilles et au noeuds internes d'être de type différents *)

let arbre1 = NoeudInterne(NoeudInterne(NoeudInterne(Feuille(8), '+', Feuille(2))), '*', Feuille(3))

(* arbre2 impossible *)

let profondeurMin arbre = match arbre with
  |Noeud(Vide, Vide) -> 0
  |Noeud(arbre1, arbre2) -> 1 + min(profondeurMin arbre1, profondeurMin arbre2);;

type 'a arbre = Vide | Feuille of 'a | Noeaud of 'a * 'a arbre * 'a arbre;;

let rec nFeuilles arbre = match arbre with
  |Feuille(_) -> 1
  |Noeud(a, a1, a2) -> nFeuilles a1 + nFeuilles a2;;

let cheminement a =
  let rec aux acc a = match a with
    |Vide -> 0
    |Noeud(Vide, Vide) -> acc
    |Noeud(a1, a2) -> aux (acc + 1) a1 + aux (acc + 1) a2
  in aux 0 a;;

let rec Strahler a =
  |Vide -> 0
  |Noeud(Vide, Vide) -> 1
  |Noeud(a1, a2) -> let u = Strahler in let v = Strahler in if u = v then u + 1 else max u v;;

(* nb de Stahler min: arbre avec une branche principale et dont les embranchements font intervenir cette branche et des feuilles *)
(* ce sont des arbres filiformes *)

(* dém théoritque sur les arbres => réaction sur la hauteur *)

(* il y a h - 1 embranchements sur un arbre filiforme et pour chaque embranchement, on choisit de mettre une feuille à droite ou à gauche *)

type 'a arbre = Vide | Noeud of 'a * 'a arbre * 'a arbre;;

let rec prefixe a = match a with
  |Vide -> []
  |Noeud(x, a1, a2) -> a::(prefixe a1)@(prefixe a2);;

let rec postfixe a = match a with
  |Vide -> []
  |Noeud(x, a1, a2) -> (preifxe a1)@(prefixe a2)@[x];;

let rec postfixe a = match a with
  |Vide -> []
  |Noeud(x, a1, a2) -> (preifxe a1)@[x]@(prefixe a2);;

let rec racines l = match l with
  |[] -> []
  |[]::q -> racines q
  |Noeud(a, a1, a2)::q -> a::(racines q);;

let rec fils l = match l with
  |[] -> []
  |[]::q -> racines q
  |Noeud(a, a1, a2)::q -> a1::a2::(racines q);;

a = Noeud(true, Noeud(false, Vide, Noeud(true, Vide, Vide)), Noeud(false, Vide, Noeud(false, Vide, Noeud(true, Vide, Vide))))

let rec cherche n E =
  |Noeud(a, Vide, Vide) -> false
  |Noeud(a, a1, a2) -> if a = x then true else (if a mod 2 = 0 then cherche n / 2 a1 else cherche n / 2 a2);;

(* recherche d'un élement dans une liste: 0(n) *)
(* recherche d'un élement dans un  arbre radix: log_2(x) où x est le nb de chiffre dans l'écriture en base 2 *)

let rec cherche n e = match n, e with
  |0 , Noeud(h, g, d) -> h
  |a, Noeud()

(* Révisions *)

type arbre = Vide | Noeud of arbre * arbre;;
