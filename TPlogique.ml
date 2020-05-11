type prop =
    |Vrai
    |Faux
    |Var of string
    |Non of prop
    |Ou of prop * prop
    |Et of prop * prop
    |Xor of prop * prop
    |Imp of prop * prop
    |Eq of prop * prop 

let rec assoc x l = match l with 
  |(s, b)::q when s = x -> b 
  |t::q  -> assoc x q 
  |[] -> failwith "x n'est pas dans la liste";;

let rec evalue l prop = match prop with
  | Vrai -> true
  | Faux -> false
  | Var(x) -> assoc x l
  | Non(p) -> not(evalue l p)
  | Ou(p1, p2) -> ((evalue l p1) || (evalue l p2))
  | Et(p1, p2) -> ((evalue l p1) && (evalue l p2))
  | Imp(p1, p2) -> (not(evalue l p1) || (evalue l p2))
  | Eq(p1, p2) -> ((not(evalue l p1) || (evalue l p2))&&(not(evalue l p2) || (evalue l p1)))
  | Xor(p1, p2) -> (((evalue l p1)||(evalue l p2))&&(not((evalue l p1)&&(evalue l p2))));;

(* NAND = NON (P ET Q) *)

type nand_prop = VAR of string | NAND of nand_prop * nand_prop;;

let rec prop_to_nand prop = match prop with
  | Non(p) -> NAND((prop_to_nand p),(prop_to_nand p))
  | Et(p1, p2) -> NAND(NAND((prop_to_nand p1),(prop_to_nand p2)),NAND((prop_to_nand p1),(prop_to_nand p2)))
  | Ou(p1, p2) -> NAND(NAND((prop_to_nand p1),(prop_to_nand p1)),NAND((prop_to_nand p2),(prop_to_nand p2)))
  | Xor(p1, p2) -> prop_to_nand(Et(Ou(p1,p2),Non(Et(p1,p2))))
  | Imp(p1, p2) -> prop_to_nand(Ou(Non(p1),p2))
  | Eq(p1, p2) -> prop_to_nand(Et(Imp(p1, p2),Imp(p2, p1)))
  | Var(x) -> VAR(x)
  | Vrai -> let p = Var("lol") in
        prop_to_nand(Ou(Non(p),p))
  | Faux -> prop_to_nand(Non(Vrai));;

((evalue [] Vrai) = true);;

(* Test Faux *)
((evalue [] Faux) = false);;

(* Test Var *)
((evalue [("x",true)] (Var "x")) = true)
&&
((evalue [("x",false)] (Var "x")) = false);;


(* Test Non *)
((evalue [] (Non Faux)) = true)
&&
((evalue [] (Non Vrai)) = false);;

(* Test ou *)
((evalue [] (Ou (Vrai, Vrai))) = true)
&&
((evalue [] (Ou (Vrai, Faux))) = true)
&&
((evalue [] (Ou (Faux, Vrai))) = true)
&&
((evalue [] (Ou (Faux, Faux))) = false) ;;


(* Test et *)
((evalue [] (Et (Vrai, Vrai))) = true)
&&
((evalue [] (Et (Vrai, Faux))) = false)
&&
((evalue [] (Et (Faux, Vrai))) = false)
&&
((evalue [] (Et (Faux, Faux))) = false) ;;


(* Test Xor *)
((evalue [] (Xor (Vrai, Vrai))) = false)
&&
((evalue [] (Xor (Vrai, Faux))) = true)
&&
((evalue [] (Xor (Faux, Vrai))) = true)
&&
((evalue [] (Xor (Faux, Faux))) = false) ;;

(* Test Imp *)
((evalue [] (Imp (Vrai, Vrai))) = true)
&&
((evalue [] (Imp (Vrai, Faux))) = false)
&&
((evalue [] (Imp (Faux, Vrai))) = true)
&&
((evalue [] (Imp (Faux, Faux))) = true) ;;

(* Test Eq *)
((evalue [] (Eq (Vrai, Vrai))) = true)
&&
((evalue [] (Eq (Vrai, Faux))) = false)
&&
((evalue [] (Eq (Faux, Vrai))) = false)
&&
((evalue [] (Eq (Faux, Faux))) = true) ;;

let rec mem x l = match l with 
  |t::q when t = x -> true 
  |t::q -> mem x q 
  |[] -> failwith "element non present dans la liste";;

let rec liste_var p = match p with
  |Var(x) -> [x]
  |Vrai -> [] 
  |Faux -> [] 
  |Non(p) -> liste_var p 
  |Et(p1, p2) |Ou(p1, p2) |Imp(p1, p2) |Xor(p1, p2) |Eq(p1, p2) -> (liste_var(p1))@(liste_var(p2))

let rec substitue prop x b = match prop with
  |Var(y) when (x = y) -> if b=true then Vrai else Faux
  |Non(p) -> Non(substitue p x b)
  |Et(p1, p2) -> Et(substitue p1 x b, substitue p2 x b)
  |Ou(p1, p2) -> Ou(substitue p1 x b, substitue p2 x b)
  |Eq(p1, p2) -> Eq(substitue p1 x b, substitue p2 x b)
  |Imp(p1, p2) -> Imp(substitue p1 x b, substitue p2 x b)
  |Xor(p1, p2) -> Xor(substitue p1 x b, substitue p2 x b)
  |p -> p;;

let tautologie prop = 
  let l = liste_var prop in 
  let n = List.length l in 
  let rec aux l prop =
