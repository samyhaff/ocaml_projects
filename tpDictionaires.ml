type dico = Nil | Noeud of char * dico * dico;;

let chercher m d = 
  let n = String.legth m in
  let rec aux d i = match d with 
    |Nil -> false
    |Noeud(x, dg, dd) when x = m[i] -> (aux dg (i + 1)) || (aux dd (i + 1))
    |Noeud(x, dg, dd) -> false
    |Noeud(x, dg, dd) when (i = m - 1) && (x = '.') -> true

