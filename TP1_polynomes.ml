(* TP Polynomes *)

(* Q2 *)
(* des que la liste possede au moins deux elements, on en place un dans chaque liste renvoyee *)

let rec division l = match l with
	| [] -> [], []
	| [x] -> [], [x]
	| x::y::q -> let l1, l2 = division q in
					(x::l1, y::l2);;


division [1;2;3;4;5;6];;
division [1;2;3;4;5];;


(* Q3 *)
(* on parcourt les deux listes en placant a chaque fois l'element le plus petit en tete *)

let rec fusion_int l1 l2 = match (l1, l2) with
	| [], l -> l
	| l, [] -> l
	| x::q1, y::q2 when x <=y -> x::(fusion_int q1 l2)
	| x::q1, y::q2 -> y::(fusion_int l1 q2);;


fusion_int [1;3;5] [2;4;6];;
fusion_int [1;3;5;7] [2;4;6];;
fusion_int [1;3;5] [2;4;6;8];;


(* Q4 *)
(* ne pas oublier les cas de base ou la liste est deja triee; on applique le Diviser-Pour-Regner lorsque la liste est non triviale *)

let rec tri_fusion_int l = match l with
	| [] -> []
	| [x] -> [x]
	| _ -> let l1, l2 = division l in
	let tri1 = tri_fusion_int l1
	and tri2 = tri_fusion_int l2 in
	fusion_int tri1 tri2;;

tri_fusion_int [4;3;7;1;2;8;5;6];;

(* Q5 *)
(* adaptation a un ordre quelconque,
ou la fonction ordre : 'a -> 'a -> bool compare deux elements de type 'a  *)

let rec fusion ordre l1 l2 = match (l1, l2) with
	| [], l -> l
	| l, [] -> l
	| x::q1, y::q2 when ordre x y -> x::(fusion ordre q1 l2)
	| x::q1, y::q2 -> y::(fusion ordre l1 q2);;


let rec tri_fusion ordre l = match l with
	| [] -> []
	| [x] -> [x]
	| _ -> let l1, l2 = division l in
	let tri1 = tri_fusion ordre l1
	and tri2 = tri_fusion ordre l2 in
	fusion ordre tri1 tri2;;


(* Q7 *)
(* fst permet d'acceder au premier element d'un couple, snd permet d'acceder au second *)

let ordre m1 m2 = snd m1 >= snd m2 ;;

(* on choisit une liste de couples *)
type polynome = (float * int) list;;

let p1 = [(2., 5) ; (3., 0) ; (4., 5) ; (1., 1) ; (2., 1) ; (0., 5) ; (-1.,1); (0., 2)];;

(* Q8 *)
(* un parcours de la liste en testant si un coefficient est nul *)

let rec elim_zero p = match p with
	| [] -> []
	| (c,d)::q when c = 0. -> elim_zero q
	| m::q -> m:: (elim_zero q);;

elim_zero p1;;

(* Q9 *)
(* la fonction auxiliaire est celle de l'indication de l'enonce *)

let contraction p =
let rec contraction_aux (c,d) p = match p with
	| [] -> [(c,d)]
	| (c',d')::q when d=d' -> contraction_aux (c +. c', d) q
	| (c',d')::q -> (c,d)::(contraction_aux (c',d') q)
in
match p with
	| [] -> []
	| t::q -> contraction_aux t q;;

let p1_tri = tri_fusion ordre p1;;
contraction p1_tri;;

(* Q10 *)
(* la contraction peut faire apparaitre des zeros *)

let normalisation p =
elim_zero (contraction (tri_fusion ordre p));;

let p1_norm = normalisation p1;;

(* Q11 *)
(* le polynome est sous forme canonique, donc le premier monome de la liste est le coefficient dominant *)

let degre p = match p with
| [] -> -1
| (c,d)::_ -> d;;

degre p1_norm;;

(* Q12 *)

(* on maintient l'invariant sur la forme canonique du polynome renvoy�, il faut additionner les coefficients de meme degre et le supprimer s'il devient nul *)

let rec addition p1 p2 = match (p1, p2) with
| [] , _ -> p2
| _ , [] -> p1
| (c1,d1)::q1, (c2,d2)::q2 ->
	if d1 = d2 	then let c= c1 +. c2 in
		if c = 0. 	then addition q1 q2
						else (c,d1)::(addition q1 q2)
					else
		if d1 > d2	then (c1,d1)::(addition q1 p2)
						else (c2,d2)::(addition p1 q2);;


let p2 = [(3. , 5) ; (4.,1) ; (3. , 0) ];;
let p3 = [(5. , 7) ; (1.,5) ; (2. , 0) ];;

addition p2 p3;;

(* Q13 *)
(* chaque coefficient est multiplie par c, et on ajoute d a chaque degre, ce qui est fait ici par la fonction map *)

(* Rappel *)
(* (List.map f l) transforme la liste [a1;a2;...;an] en la liste [f(a1) ; f(a2) ; ... ; f(an)] *)
(*
let rec map f l = match l with
	| [] -> []
	| t::q -> (f t):: (map f q);;
*)

let mult_monome (c,d) p =
if c = 0. 	then []
				else List.map (fun (c',d') -> (c *. c', d+d')) p;;

(* Q14 *)
(* en notant p1 = m + q, on a
p1 * p2 = m*p2 + q*p2
q*p2 est la multiplication naive de deux polynomes
m*p2 est la mult. d'un polynome par un monome
on additionne les deux polynomes
*)

(* comme mult_monome et addition preservent la canonicite des polynomes, mult_naive aussi garantit un polynome sous forme canonique a la sortie *)

let rec mult_naive p1 p2 =
match p1 with
	| [] -> []
	| m::q -> let r = mult_monome m p2 in
	addition r (mult_naive q p2);;

mult_naive p2 p3;;

(* Q18 *)

(* On ecrit les trois fonctions d'un algorithme Diviser-Pour-Regner :
division, combinaison et la fonction recursive finale
*)

(*
Etant donne un polynome p et un entier n, (division_kara n p) renvoie deux polynomes (p1,p0) tels que
p = X^n * p1 + p0
*)
(* En pratique, on utilisera cette fonction avec p et n/2 ou n est le degre de p
*)

let rec division_kara n p =
match p with
	| [] -> [], []
	| (c,d)::q when d<n -> [], p
	| (c,d)::q -> let p1, p0 = division_kara n q in
					(c, d-n)::p1, p0;;



(*
(combinaison n r2 r1 r0)
renvoie r2 * X^(2n) + (r1 - (r0 +r2)) * X^n + r0
*)

let combinaison n r2 r1 r0 =
let milieu = addition r1 (mult_monome (-1., 0) (addition r2 r0))
in
addition (mult_monome (1., 2*n) r2)
(addition (mult_monome (1., n) milieu) r0);;

(*
la fonction finale effectuant le decoupage, les trois produits et la combinaison *)

let rec mult_kara p q = match p,q with
| [], _ -> []
| _ , [] -> []
| [m], _ -> mult_monome m q
| _, [m] -> mult_monome m p
| _, _ -> let n = (max (degre p) (degre q)) / 2 in
let p1, p0 = division_kara (n+1) p
and q1, q0 = division_kara (n+1) q
in
let r2 = mult_kara p1 q1
and r1 = mult_kara (addition p1 p0) (addition q1 q0)
and r0 = mult_kara p0 q0
in
combinaison (n+1) r2 r1 r0;;


mult_kara p2 p3;;
