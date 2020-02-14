(* liste d'adjacence *)

let calc_deg_sortant g = 
  let n = Array.length g in 
  let deg_sortant = Array.make n 0 in 
    for i = 0 to n do 
      deg_sortant.(i) <- (List.length g.(i)) 
    done;
    deg_sortant;;

let calc_deg_entrant g =
  let n = Array.length g in 
  let deg_entrant = Array.make n 0 in 
    for i = 0 to n - 1 do 
      let rec aux l = match l with 
        |[] -> ()
        |t::q -> deg_entrant.(t) <- deg_entrant.(t) + 1; aux q
      in aux g.(i)
    done;
    deg_entrant;;

let l = calc_deg_entrant [|[];[0];[0]|];;

(* matrice d'adjacence *)

let calc_deg_entrant2 g = 
  let n = Array.length g in 
  let deg_entrant = Array.make n 0 in 
    for i = 0 to n - 1 do 
      let compteur = ref 0 in 
        for j = 0 to n - 1 do 
          if g.(i).(j) = 1 then 
            compteur := !compteur + 1
        done;
        deg_entrant.(i) <- !compteur;
    done;
    deg_entrant;;

let calc_deg_sortant2 g = 
  let n = Array.length g in 
  let deg_sortant = Array.make n 0 in 
    for i = 0 to n - 1 do 
      let compteur = ref 0 in 
        for j = 0 to n - 1 do 
          if g.(j).(i) = 1 then 
            compteur := !compteur + 1
        done;
        deg_sortant.(i) <- !compteur;
    done;
    deg_sortant;;

let transposee g = 
  let n = Array.length g in 
  let gt = Array.make n [] in 
    for i = 0 to n - 1 do 
      let rec aux l i = match l with 
        |[] -> () 
        |t::q -> gt.(t) <- i::gt.(t); aux q i
      in aux g.(i) i
    done;
    gt;;

let g = transposee [|[1;2];[0];[]|];;

let voisins g i = 
  let n = Array.length g in
  let l = ref [] in 
    for j = 0 to n - 1 do 
      if g.(i).(j) = 1 then
        l := j::(!l)
    done;
    !l;;

let dfs g = 
  let n = Array.length g in 
  let vus = Array.make n false in 
  let rec bfsAux g aVisiter acc = match aVisiter with 
    |[] -> acc 
    |t::q -> if not (vus.(t)) then 
          begin 
            vus.(t) <- true;
            bfsAux g (q@(voisins g t)) (acc@[t])
          end
        else
          bfsAux g q acc;
  in bfsAux g [0] [];;

let cycle g = 
  let test = ref false in
  let n = Array.length g in 
  let vus = Array.make n false in 
  let rec aux g aVisiter = match aVisiter with 
    |[] -> ()
    |t::q -> if (vus.(t)) then test := true 
        else 
          begin
            vus.(t) <- true;
            aux g (q@(g.(t))) 
          end
  in aux g [0]; test;;
