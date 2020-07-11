(* très moche, à faire en récursif *)

let calculer_peres racine fils freres = 
  let n = Array.length fils in
  let peres = Array.make n (-1) in
    for i = 0 to n - 1 do 
      if fils.(i) != -1 then
        begin
          let j = ref fils.(i) in 
            peres.(!j) <- i;
            while freres.(!j) <> -1 do
              peres.(freres.(!j)) <- i;
              j := freres.(!j)
            done;
        end;
    done;
    peres;;

let calculer_arites fils freres = 
  let n = Array.length fils in 
  let arites = Array.make n 0 in 
  let rec aux i = function 
    | -1 -> ()
    |j -> arites.(i) <- arites.(i) + 1; aux i freres.(j)
  in 
    for i = 0 to n - 1 do
      aux i fils.(i)
    done;
    arites;;

let rec inserer table nb d = match nb with 
  |0 -> table.(0) <- d
  |_ when table.(nb - 1) >= d -> table.(nb) <- d
  |_ -> table.(nb) <- table.(nb - 1); inserer table (nb - 1) d;;

let calculer_Prifer racine fils freres = 
  let arites = calculer_arites fils freres in 
  let peres = calculer_peres racine fils freres in 
  let n = Array.length fils in 
  let feuilles = Array.make n 0 in 
  let prufer = Array.make n 0 in
  let nb = ref 0 in
    for i = 0 to n - 1 do 
      if fils.(i) = -1 then 
        begin
          inserer feuilles !nb i;
          nb := !nb + 1
        end;
    done;
    for k = 0 to n - 2 do 
      let f = feuilles.(!nb - 1) in 
      let p = peres.(f) in 
        prufer.(k) <- p;
        arites.(p) <- arites.(p) - 1;
        if arites.(p) = 0 then inserer feuilles !nb p 
        else nb := !nb - 1
    done;
    prufer;;

(* pour calculer_arites_par_Prufer il suffit de compter *)
