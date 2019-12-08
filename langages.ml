(* mot de dyck *)

type alphabet = A | B;;

let dyck m =
  let rec aux acc w = match w with
    |[] -> acc = 0
    |t::q -> if t = A then (acc >= 0) && (aux (acc + 1) q) else (acc >= 0) && (aux (acc - 1) q)
  in aux 0 m;;

let facteurs m =
  let rec aux acc n w = match w with
    |[] -> n
    |B::q when acc = 1 -> aux 0 (n + 1) q
    |B::q -> aux (acc - 1) n q
    |A::q -> aux (acc + 1) n q
  in aux 0 0 m;;

let factorisation m =
  let rec aux acc w = match w with
    |[] -> ()
    |[B] -> print_char ")"
    |B::q when acc=1 -> print_string ")." ; aux 0 q
    |B::q -> print_char ")" ; aux (acc-1) q
    |A::q -> print_char "()" ; aux (acc+1) q
  in aux 0 m;;

let string_to_list s =
  let n = String.length s in
    let rec aux k l = match k with
    | x when x=n -> l
    |_ when s.[k] = "a" -> A::(aux (k+1) l)
    |_ when s.[k] = "b" -> B::(aux (k+1) l)
    |_ -> failwith "erreur syntaxe a b"
  in aux 0 [] ;;

let test = "aabb";;
dyck test;;
facteurs test;;
factorisation test;;

let longueur liste =
  let rec aux acc liste = match liste with
    |[] -> acc
    |t::q -> aux (acc + 1) q
  in aux 0 liste;;

let sous_liste l k long =
  let rec aux acc compteur liste = match liste, compteur with
    |[], _ -> acc
    |t::q, n when n >= k && n <= k + long - 1 -> aux (acc @ [t]) (n + 1) q
    |t::q, n -> aux acc (n + 1) q
  in aux [] 0 l;;

let estCarre w =
  let n = longueur w in
  if n mod 2 = 1 then false
  else sous_liste w 0 (n / 2) = sous_liste w (n / 2) (n / 2);;

let contientRepetitionAux w m =
  let n = longueur w in
  let compteur = ref 0 in
  for i = 0 to n / m
  do
    if estCarre (sous_liste w i m) then compteur := !compteur + 1
  done;
  !compteur <> 0;;

let contientRepetition w =
  let compteur = ref 0 in
  let n = longueur w in
  for i = 2 to n / 2 + 1
  do
    if contientRepetitionAux w i then compteur := !compteur + 1
  done;
  !compteur <> 0;;

(* sijet Mines-Ponts *)

let est_present m t s =
let lm = string_length m in
let b = ref true and i = ref 0 in while !b&& !i<lmdo
b :=m.[ !i]=t.[s+( !i)] ;
i := !i+1 ; done ;
!b ;;

let positions m t = let rec aux m t s = if s=(-1) then []
else
if est_present m t s
then s : :(aux m t (s-1)) else (aux m t (s-1))
in
aux m t (string_length t - string_length m) ;;
