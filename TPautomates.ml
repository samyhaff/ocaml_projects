type 'a automate = { mutable taille : int ;
                     mutable initial : int ;
                     transitions : ('a * int) list array ;
                     final : bool array };;

let calcul_complet x a = 
  let n = String.length x in
  let rec aux i q = match i with 
    |i when i = n -> q
    |i -> aux (i + 1) (List.assoc x.[i] a.transitions.(q)) 
  in a.final.(aux 0 a.initial);; 

let a0 = {taille = 5 ; initial = 0 ; transitions = [|[('a', 1); ('b', 2)]; [('a', 1); ('b', 2)]; [('a', 3); ('b', 4)]; [('a', 2); ('b', 4)]; [('a', 3); ('b', 2)]|] ; final = [|false; true; false; true; false|] };;

calcul_complet "aa" a0;;
calcul_complet "aba" a0;;
calcul_complet "bab" a0;;

let rec mon_assoc c l = match l with 
  |[] -> (false, -1)
  |(x, q)::_ when x = c -> (true, q)
  |t::q -> mon_assoc c q;;

let calcul_det x a = 
  let n = String.length x in 
  let i = ref 0 in 
  let q = ref a.initial in 
  let flag = ref true in 
    while (!i < n && !flag) do
      let b, e = mon_assoc x.[!i] a.transitions.(!q) in 
        if not b then flag := false 
        else 
          begin
            q := e;
            i := !i + 1
          end;
    done;
    (!flag) && a.final.(!q);;

a1 = {taille = 5 ; initial = 0 ; transitions = [|[('a', 1); ('b', 2)]; [('a', 1); ('b', 2)]; [('a', 3); ('b', 4)]; [('a', 2); ('b', 4)]; [('a', 3); ('b', 2)]|] ; final = [|false; true; false; true; false|] };;

calcul_det "aa" a1;;
calcul_det "aba" a1;;
calcul_det "bab" a1;;

let complete a = 
  let transitions = Array.append a.transitions [|[]|] in 
  let n = Array.length a.transitions in 
    for i = 0 to n - 1 do 
      if not (fst (mon_assoc 'a' a.transitions.(i))) then 
        transitions.(i) <- ('a', n)::a.transitions.(i) 
      else if not (fst (mon_assoc 'b' a.transitions.(i))) then 
        transitions.(i) <- ('b', n)::a.transitions.(i) 
    done;
    {taille = n + 1 ; initial = a.initial ; transitions = transitions ; final = a.final };;

complete a1;;

let rec retirer l x = match l with 
  |[] -> failwith "liste vide"
  |[x] -> []
  |t::q when t = x -> q
  |t::q -> t::(retirer q x);;

let rec retirer_plusieurs l l' = match l' with  
  |[] -> l
  |t::q -> retirer_plusieurs (retirer l t) q;;

let accessible a = 
  let rec aux vus l = match l with 
    |[] -> vus
    |t::q -> let d = List.map snd a.transitions.(t)
        in let d' = retirer_plusieurs d vus 
        in aux (d' @ vus) (d' @ q)
  in aux [a.initial] [a.initial];;

let rec mon_assoc c l = match l with 
  |[] -> (false, -1)
  |(x, q)::_ when x = c -> (true, q)
  |t::q -> mon_assoc c q;;

let rec mon_assoc_2 c l = match l with 
  |[_] -> []
  |(x, q)::f when x = c -> q::(mon_assoc_2 c f)
  |t::q -> mon_assoc_2 c q 

let calul_nondet x b = 
  let a = complete b in
  let n = String.length x in 
  let rec aux q i = match i with
    |i when i = n -> q
    |i -> aux (accessible_2 q x.[i]) (i + 1)
  in aux [a.initial] 0;;

let a3 = {taille = 5 ; initial = 0 ; transitions = [|[('a', 1); ('a', 2); ('b', 2)]; [('a', 1)]; [('a', 3); ('b', 4)]; [('b', 4)]; [('a', 3)]|] ; final = [|false; true; false; true; false|] };;

calcul_nondet "aa" a3;;














