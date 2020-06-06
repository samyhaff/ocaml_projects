(* TP Dictionnaire *)

let string_of_char = String.make 1 ;;
let char_of_string s = s.[0];;

type dico = Nil | Noeud of char * dico * dico ;;
let d = Noeud
  ('b',
   Noeud
    ('a',
     Noeud
      ('c', Noeud ('.', Nil, Nil),
       Noeud
        ('r', Noeud ('.', Nil, Nil),
         Noeud
          ('s', Noeud ('.', Nil, Noeud ('e', Noeud ('.', Nil, Nil), Nil)),
           Nil))),
     Nil),
   Noeud
    ('r',
     Noeud
      ('a',
       Noeud
        ('s', Noeud ('.', Nil, Nil), Noeud ('t', Noeud ('.', Nil, Nil), Nil)),
       Noeud ('i', Noeud ('z', Noeud ('.', Nil, Nil), Nil), Nil)),
     Nil));;



let chercher m a =
  let n = String.length m in
  let rec aux k b = match b with
    | Nil                             -> false
    | Noeud ('.', _, _) when k = n    -> true
    | Noeud (_, _, a2)  when k = n    -> aux k a2 
    | Noeud (c, a1, _) when m.[k] = c -> aux (k+1) a1
    | Noeud (_, _, a2)                -> aux k a2
  in aux 0 a ;;




let branche m = 
  let n = String.length m in
  let rec aux i = match i with
    | k when k = n -> Noeud ('.', Nil, Nil)
    | k            -> Noeud (m.[k], aux (k+1), Nil)
  in aux 0 ;;



let inserer m a =
  let n = String.length m in
  let rec aux k b = match b with
    | Nil                                 -> branche (String.sub m k (n-k))
    | Noeud ('.', a1, a2) as a when k = n -> a
    | Noeud (c, a1, a2) when k = n        -> Noeud (c, a1, aux k a2)
    | Noeud (c, a1, a2) when m.[k] = c    -> Noeud (c, aux (k+1) a1, a2)
    | Noeud (c, a1, a2)                   -> Noeud (c, a1, aux k a2)
  in aux 0 a ;;



let rec creer l = match l with 
  | []   -> Nil
  | t::q -> inserer t (creer q) ;;




let extraire a =
  let rec aux acc b = match b with
    | Nil                -> []
    | Noeud ('.', _, a2) -> acc::(aux acc a2)
    | Noeud (c, a1, a2)  -> (aux (acc ^ (string_of_char c)) a1) @ (aux acc a2)
  in aux "" a ;;



let liste_ex  = ["bac"; "bar"; "bas"; "base"; "ras"; "rat"; "riz"] ;;


creer liste_ex ;;
extraire d;;





