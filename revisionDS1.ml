open List;;

let rec n_eme liste f n = match liste, n with
  |[], k -> failwith "n'existe pas"
  |t::q, 1 when f t -> t
  |t::q, k when f t -> n_eme q f (n - 1);;

let rec suppr liste n = match liste, n with
  |[], _ -> []
  |t::q, 1 -> q
  |t::q, k -> t::suppr q (k - 1);;

let rec insere liste a n = match liste, n with
  |[], _ -> [a]
  |l, 1 -> a::l
  |t::q, k -> t::insere q (k - 1);;

let rec prefixe l = match l with
  |[] -> []
  |t::q -> [t]::map (function x -> t::x) (prefixe q);;

type reelsEtendus =
  |Reel of float
  |PlusInfini
  |MoinsInfini;;

type complexe = {Re: float; Im: float};;

type entierNaturel = Zero | Successeur of entierNaturel;;

type 'a monome == int * 'a;;
type 'a polynome == ('a monome) list;;

(* DS d'entrainement *)

let rec unMystere l = match l with
  |[] -> [], []
  |(a, b)::q -> let q1, q2 = unMystere q in (a::q1, b::q2);;

let nb0cc t ob i j =
  let rec nbOccAux t ob i j c x =
    if x = j then c
    else if t.(x) = ob then nbOccAux t ob i j (c + 1) (x + 1)
    else nbOccAux t ob i j c (x + 1)
  in nbOccAux t ob i j 0 i;;

type mot == int list;;

let luka l =
  let rec lukaAux l acc = match l with
    |[] -> (acc = -1)
    |t::q -> acc >= 0 && (lukaAux q (acc + t))
  in lukaAux l 0;;

(* selection sort version listes *)

let rec selectMin liste = match liste with
  |[a] -> a, []
  |[t::q] -> let u,v = selectMin q in if t < u then t, q else u, t::v;;

let rec triSelection liste = match liste with
  |[] -> []
  |t::[] -> [t]
  |t::q -> let u, v = selectMin q in if t < u  then t::triSelection q else u::triSelection(t::v);;

(* insertion sort listes *)

let rec insere l x = match l with
  |[] -> [x]
  |t::q -> if x >= t then t::x::q else insere q x;;

let rec triInsertion liste = match liste with
  |[] -> []
  |t::q -> insere t (triInsertion q);;

let scinde l =
  let rec scindeAux l n = match l with
    |1::q -> scindeAux q (n + 1)
    |q -> n, q
  in scindeAux t 0;;

let rec composition_suivante l =
  let n, q = scinde l in
  match q with
  |[] -> []
  |h::t -> (n + 1)::(h - 1)::t;;

let rec ajoute k l = match l with
  |[] -> []
  |t::q -> k::(t::(ajoute k q));;

let rec parties1 n = match n with
  |0 -> [[]]
  |n -> (parties1 n - 1) @ (ajoute k (parties1 n - 1));;

(* selection sort vect *)

let minAux t i =
  let n = Array.length in
  if i = n - 1 then i
  else
    begin
      let u = minAux t (i - 1) in
      if t.(u) < t.(i) then u else i;
    end;;

let triSelection t =
  let n = Array.length in
  let rec triSelectionAux t i =
    if i < 1 then ()
    else
      begin
        let ind = minAux t i in
        echange t i ind;
        triSelectionAux t (i + 1)
      end;
    in triSelectionAux t 0;;

let rec enleveH l = match liste with
  |t::q as l when est_paritaire -> l
  |t::q when t.Sexe = M -> enelveH q
  |t::q -> t::(enleveH q);;

(* insertion sort vect *)

let rec echange t i j =
  let u = t.(j) in t.(j) -> t.(i); t.(i) <- u;

  let rec insere t i =
    if i = 0 then ()
    else if t.(i) < t.(i - 1) then
      begin
        echange t i (i - 1);
        insere t (i - 1)
      end;;

let triInsertion t =
  let n = Array.lenght t in
  let rec triInsertionAux t i =
    if i = n  then ()
    else
      begin
        insere t i;
        triInsertionAux t (i - 1);
      end;
    in triInsertionAux t 0;;
