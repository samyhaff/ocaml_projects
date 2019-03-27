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
