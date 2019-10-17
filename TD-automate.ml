open String;;
open List;;

type 'a automate = { taille : int ;
                     initial : int ;
                     transitions : ('a * int) list array ;
                     final : bool array } ;;

(* let a0 = { taille = 5; initial = 0; transitions = [|[("a", 1); ("b, 2")]; [("a", 1); ("b", 2)]; [("a", 3); ("b", 4)]; [("a", 2); ("b", 4)]; [("a", 3); ("b", 2)]|]};; *)
(* a0.transitions.(0) *)

let calcule_complet mot automate =
  let len = String.length mot in
  let etat = ref 0 in
  for i = 0 to len - 1 do
    let lettre = mot.[i] in
    etat := List.assoc lettre transitions.(!etat)
  done;
  automate.final.(etat);;

let rec mon_assoc c liste = match liste with
  |[(caractere, etat)] -> if caractere = c then (true, etat) else (false, etat)
  |(caractere, etat)::q -> if caractere = c then (true, etat) else mon_assoc c q;;

let calcul_det mot automate =
  let len = String.length mot in
  let etat = ref 0 in
  let u = ref true in
  let i = ref 0 in
  while !u || !i <= len - 1 do
    let lettre = mot.[i] in
    let a, b = mon_assoc lettre transitions.(!etat) in
    begin
      !u := a;
      etat := b;
      i := !i + 1
    end;
  done;
  if not !u then false else automate.final.(!etat);;

let complete automate =
  let puit automate =
    let taille = automate.taille in
    let t = concat transitions [("a", taille); ("b", taille)] in
    for i = 0 to Array.length t - 1 do
      if length automate.transitions.(i) = 1 then
        t.(i) <- automate.transitions.(i) @ (, taille )
        with
        | _ -> failwith "Unknown"
  in new_transitions = puit automate in
  let automate_det = {taille = automate.taille + 1; initial = automate.initial; transitions = new_transitions; final = automate.final }
