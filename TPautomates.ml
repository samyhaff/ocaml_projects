let rec binaire_faible n = match n with
  |0 -> []
  |n -> (n mod 2)::(binaire_faible (n / 2));;

let binaire_fort x = 
  let rec aux x acc = match x with 
    |0 -> acc
    |x -> aux (x / 2) ((x mod 2)::acc)
  in aux x [];;

type ('a, 'b) afd = {init: 'a; accept: 'a list; delta: (('a * 'b) * 'a) list};;

let rec mem elt liste = match liste with
  |[] -> false
  |t::q when t = elt -> true
  |t::q -> mem elt q;;

let rec mem_fst x l = match l with 
  |[] -> false 
  |(a, b)::_ when a = x -> true
  |(a, b)::q -> mem_fst x q;;

let rec assoc x l = match l with 
  |[] -> failwith "element non présent"
  |(a, b)::_ when a = x -> b 
  |_::q -> assoc x q;;

let reconnu aut mot = 
  let rec aux mot q = match mot with  
    |[] -> q
    |u::v when mem_fst (q, u) aut.delta -> aux v (assoc (q, u) aut.delta)
    |u::v -> false
  in mem (aux mot aut.init) aut.accept;;



let genere_fort d = 
  let transitions = ref [] in 
    for b = 0 to d - 1 do 
      transitions := ((b, 0), (2 * b) mod d)::(((b, 1), (2 * b + 1) mod d)::(!transitions))
    done;
    {init = 0; accept = [0]; delta = !transitions};;

type ('a, 'b) afnd = {nd_init : 'a list; nd_accept : 'a list; nd_delta : (('a * 'b) * 'a) list};;

let genere_faible d aut = 
  let rec aux l = match l with 
    |[] -> []
    |((q1, u), q2)::q -> ((q2, u), q1)::(aux q)
  in {nd_init = aut.accept; nd_accept = [aut.init]; nd_delta = (aux aut.delta)};;

let rec exists p l = match l with 
  |[] -> false
  |x::q when p x -> true
  |_::q -> exists p q;;

let reconnu2 a mot =
  let rec aux e m delta = match (m, delta) with
    | ([],_) -> mem e a.nd_accept
    | (_,[]) -> false
    | (a1::m1, ((q, c), r)::v) when q = e && c = a1 -> aux r m1 a.nd_delta || aux e m v | (m, _::v) -> aux e m v
  in exists (function e -> aux e mot a.nd_delta) a.nd_init ;;












