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

let rec permutation t = 
