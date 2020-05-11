let rec flat l = match l with
  |[] -> []
  |t::q -> t@(flat q);;

let rec intervales a b = match a,b with
  |a,b when a > b -> []
  |a, b -> a::(intervales (a + 1) b);;

let liste_of_bande h g d m =
  let l = ref [] in
    for j = g to d do
      l:= m.(h).(j)::(!l)
    done;
    l;;

let liste_of_bande h g d m =
  let l = (intervalle g d) in
  let rec aux l1 = match l1 with
    | [] -> []
    | t::q -> m.(h).(t)::aux q
  in aux l;;

let m = Array.make_matrix 9 9 0;;
liste_of_bande 2 2 4 m;;

let list_of_zone h b g d m = 
  let l = ref [] in 
    for i = h to b do 
      for j = g to d do
        l := m.(i).(j)::(!l)
      done;
    done;
    !l;;

let list_of_bloc k m = 
  list_of_zone (3*(k/3)) (3*(k/3)+3) (3*(k mod 3)) (3*(k mod 3)+3) m;;

let rec insertion x l = match l with 
  |[] -> [x]
  |t::q when (x <= t) -> x::l
  |t::q -> t::(insertion x l);;

let rec tri_insertion l = match l with 
  |[] -> []
  |t::q -> insertion t (tri_insertion q);;

let bonne_liste l = 
  let l2 = tri_insertion l in
  let rec test k l2 v = match k, l with
    |9, [] -> v
    |k, t::q -> test (k + 1) q (t = k and v)
  in test 1 l2 true;;

let bon_sudoku m = 
  let condition = ref false in 
    for i = 0 to 9 do 
      condition := (!condition && (bonne_liste (list_of_zone 0 8 i i m) && bonne_liste(list_of_bloc k m) && (list_of_bande 0 8 i i)))
    done;
    !condition;;








