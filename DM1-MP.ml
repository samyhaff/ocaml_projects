Open Array;

table1 = [|3; 2; 8; 6; 0; 0|]

let vider t =
  t.(0) <- 0;;

let ajouter t x =
  t.(t.(0) + 1) <- x;
  t.(0) <- t.(0) + 1;;

let concatener t1 t2 =
  t1.(0) <- t1.(0) + t2.(0);
  for i = 1 to t2.(0) do
    ajouter t1 t2.(i)
  done;;

let max_val = 99;;

let compter t =
  (* complexité: O(n) *)
  let c = Array.create (max_val + 1) 0 in
  for i = 1 to t.(0) do
    c.(t.(i)) <- c.(t.(i)) + 1
  done;
  c;;

let tri_simple t =
  (* complexité: 0(n) *)
  let compte = compter t in
  let indice = ref 1 in
  for i = 0 to max_val do
    for j = 0 to compte.(i) - 1 do
      t.(!indice) <- i;
      indice := !indice + 1
    done;
  done;;

(* donc la complexité de cette prodécudre de tri est 0(n) *)

let max_valeurs t =
  if t.(0) = 0 then -1 else
  let maxi = ref t.(1) in
  for i = 2 to t.(0) do
    if t.(i) > !maxi then
      maxi := t.(i)
  done;
  !maxi;;

(* la valeur attendue sur l'exemple est 5 puisque 8 est insignifiant *)

let nombre_chiffres x =
  int_of_float(log(float_of_int x) /. log 10.) + 1;;

let max_chiffres t =
  let maxi = max_valeurs t in
  nombre_chiffres maxi;;

let rec exp_rapide x n = match n with
  (* cet algorithme est du style divisser pour reigner et d'après le théorème maitre,
     la complexité est 0(log_2(n)) *)
  |0 -> 1
  |1 -> x
  |_ -> let u = exp_rapide x (n / 2) in if n mod 2 = 0 then u * u
    else x * u * u;;

(* puiss10 : int -> int *)
let puiss10 p = exp_rapide 10 p;;

let chiffre_de_rang r n = (n / (puiss10 (r - 1))) - (10 * (n / (puiss10 r)));;

let distribuer table r baquets =
  for i = 0 to 9 do
    baquets.(i).(0) <- 0;
  done;
  for i = 1 to table.(0) do
    let u = chiffre_de_rang r (table.(i)) in
    begin
      baquets.(u).((baquets.(u).(0)) + 1) <- table.(i);
      baquets.(u).(0) <- ((baquets.(u).(0)) + 1)
    end
  done;;

let tri_baquets table =
  (* complexité: O(maxc * n) ce qui est linéaire pour des petits nombres avec bcp de rédondance mais
     en O(n2) pour des nombres tous distincts *)
  for i = 1 to (nombre_chiffres (max_valeurs table)) do
  let baquets = Array.init 10 (fun _ -> Array.make table.(0) 0) in
    distribuer table i baquets;
    let j = ref 1 in
      for k = 0 to 9 do
        for l = 1 to (baquets.(k).(0)) do
          table.(!j) <- (baquets.(k).(l));
          j := !j + 1;
        done;
      done;
  done;;

(* Invariant de boucle: après le r-ième passage dans la boucle, tous les nombres à r chiffres sont
    correctement triés. On peut alors monter la correction de l'algorithme par induction:
    Hn: "les nombres d'au plus n chiffres sont triés"
    H1: les chiffres sont placés dans les tables selon la valeur du chiffre, lors de la concatenation des 10 baquets,
    ils seront triés
    Hn => Hn+1: Supposons Hn vraie, tous les nombres d'au plus n chiffres sont triés, ils ne seront pas pris en compte lors
    prochain pàssage de la boucle puisqu'elle triera les élements de la table selon la valeur du chiffre de rang n+1, lors de
    l'étape de concatenation, les nombres à au plus n chiffres se retrouveront devant ceux à n+1 chiffres triés. Donc,
    tous les nombres à au plus n+1 chiffres de la table sont triés.
    La correction est donc prouvée *)

let distribuer_bis table r indice_debut baquets garage =
  for i = 0 to 9 do
    baquets.(i).(0) <- 0
  done
  for i = indice_debut to table.(0) do
    let u = (chiffre_de_rang r (table.(i))) in
      if ((u = 0) && (r > (nombre_chiffres (table.(i)))))
      then begin
        garage.(0) <- (garage.(0) + 1);
        garage.(garage.(0)) <- table.(i)
      end
      else begin
      baquets.(u).((baquets.(u).(0)) + 1) <- table.(i);
      baquets.(u).(0) <- ((baquets.(u).(0)) + 1)
      end
  done;;

let tri_baquets_bis table =
  let baquets = Array.init 10 (fun _ -> Array.make table.(0) 0) in
    let garage = Array.make (table.(0) + 1) 0 in
      let garage0 = ref garage.(0) in
        let r = ref 1 in
          while garage.(0) < table.(0) do
            distribuer_bis table !r (garage.(0) + 1) baquets garage;
            let j = ref (!garage0 + 1) in
              for k = (!garage0 + 1) to garage.(0) do
                table.(k) <- garage.(k);
                j := !j + 1;
              done;
              for k = 0 to 9 do
                for l = 1 to (baquets.(k).(0)) do
                  table.(!j) <- (baquets.(k).(l));
                  j := !j + 1
                done;
              done;
              r := !r + 1
          done;;
