let rec list_map f l = match l with
  |[] -> []
  |t::q -> (f t)::(list_map f q);;

let rec list_select f l = match l with 
  |[] -> []
  |t::q when f t -> t::(list_select f q)
  |t::q -> list_select f q;;

let rec insere x l = match l with 
  |[] -> [x]
  |t::_ when x <= t -> x::l
  |t::q -> t::(insere x q);;

let rec tri_insertion l = match l with 
  |[] -> []
  |t::q -> insere t (tri_insertion q);;

let list_rev l = 
  let rec aux l acc = match l with
    |[] -> acc
    |t::q -> aux q (t::acc) 
  in aux l [];;

(* exp rapide *)

let rec puissance x n = match n with
  |0 -> 1
  |k -> x * (puissance x (k - 1));;

let rec pgcd a b = match a,b with
  |x, y when y > x -> pgcd y x
  |x, 0 -> x
  |x, y -> pgcd y (x mod y);;

let base x n = 
  let rec aux x acc = match x with 
    |0 -> acc
    |x -> aux (x / n) ((x mod n)::acc)
  in aux x [];;

let rec eval_base b l = match l with
  | [] -> 0
  | h::t -> h + b * (eval_base b t) ;;

let fibo n =
  let rec aux n acc1 acc2 = match n with
    |0 -> acc1
    |1 -> acc2
    |n -> aux (n - 1) acc2 (acc1 + acc2)
  in aux n 0 1;;

let eval poly x = eval_base x poly;;

let recherche_dicho x tab =
  let rec aux i j = 
    let y = (i + j) / 2 in 
      if x > y then aux y j 
      else if x < y then aux i y 
      else i
  in aux 0 (Array.length tab);;

type arbre = Vide | Noeud of int * arbre * arbre;;

let rec max_min a = match a with
  | Vide -> min_int, max_int
  | Noeud(x,Vide, Vide) -> x,x
  | Noeud(x,g,d) -> let max_g,min_g = max_min g and max_d,min_d = max_min d in
        (max max_g (max max_d x), min min_g (min min_d x)) ;;

let rec max_sum a = match a with
  | Vide -> 0
  | Noeud(x,g,d) -> x + max (max_sum g) (max_sum d);;

let accessible g s =
  let n = Array.length g in
  let dejavu = Array.make n false in
  let rec visite l = match l with
    | [] -> ()
    | h::t when dejavu.(h) -> visite t
    | h::t -> dejavu.(h) <- true ; visite g.(h) ; visite t
  in visite [s] ; dejavu ;;

let rec tri_rapide l0 = 
  let rec decoupe pivot l = match l with 
    |[] -> [], []
    |h::t -> let l1, l2 = decoupe pivot t in  
          if h < pivot then (h::l1, l2) else (l1, h::l2) in 
    match l0 with 
      |[] -> []
      |pivot::suite -> let l1, l2 = decoupe pivot suite in 
            (tri_rapide l1)@(pivot::(tri_rapide l2));;

let hierarchie arbre = 
  let rec boucle file = match file with 
    |[] -> []
    |(Vide)::t -> boucle t
    |(Noeud(x, g, d))::t -> x::(boucle (t @ [g; d]))
  in  boucle arbre;;

type arbre_expr = Value of int | Op_bin of string * arbre_expr * arbre_expr | Op_un of string * arbre_expr;;

let rec prefixe ex = match ex with 
  |Value v -> string_of_int v
  |Op_bin(s, g, d) -> s ^ "(" ^ (prefixe g) ^ "," ^ (prefixe d) ^ ")"
  |Op_un(s, e) -> s ^ "(" ^ (prefixe e) ^ ")";;

let ex = Op_bin("-", Value(3), Op_bin("+", Value(5), Value(2)));;
prefixe ex;;

let rec infixe ex = match ex with
  | Value v -> string_of_int v
  | Op_bin(s,g,d) -> "("^(infixe g)^")" ^ s ^ "("^(infixe d)^")"
  | Op_un(s,e) -> s ^"("^(infixe e)^")" ;;

let rec suffixe ex = match ex with
  | Value v -> string_of_int v
  | Op_bin(s,g,d) -> "("^(suffixe g)^","^(suffixe d)^")" ^s
  | Op_un(s,e) -> "("^(suffixe e)^")" ^s ;;

let membre x chaine =
  let n = String.length chaine in 
  let i = ref 0 in 
  let flag = ref false in 
    while (not !flag) && (!i < n) do
      flag := (String.make 1 chaine.[!i] = x);
      i := !i + 1
    done;
    !flag;;

let operations a b e = 
  if e = "+" then a + b
  else if e = "-" then a - b
  else if e = "*" then a * b
  else a / b;;

let eval_postfixe e = 
  let operateurs = "+-*/" in 
  let n = String.length e - 1 in 
  let s = Stack.create() in
    for i = 0 to n do
      let v = String.sub e i 1 in 
        if membre v operateurs then 
          begin 
            let a = int_of_string (Stack.pop s) in 
            let b = int_of_string (Stack.pop s) in 
            let c = operations a b v in 
              Stack.push (string_of_int c) s;
          end
        else 
          Stack.push v s
    done;
    Stack.pop s;;

(* ARBRES BINAIRES DE RECHERCHE *)

let rec cherche_abr e a = match a with
  | Vide -> false
  | Noeud(x,g,d) when x = e -> true
  | Noeud(x,g,d) when x > e -> cherche_abr e g
  | Noeud(x,g,d) -> cherche_abr e d;;

let rec ajoute v abr = match abr with 
  |Vide -> Noeud(v, Vide, Vide)
  |Noeud(x, g, d) -> if x <= v then Noeud(x, g, ajoute v d)
      else Noeud(x, ajoute v g, d);;

(* fonction auxiliare qui renvoie la plus grande valeur de l'arbre et l'arbre privé de cette valeur *)
let rec enleverDroite a = match a with 
  |Vide -> failwith "Arbre vide"
  |Noeud(x, g, Vide) -> x, g
  |Noeud(x, g, d) -> let u, dr = enleverDroite d in u, Noeud(x, g, dr);;

let rec supprime k a = match a with 
  |Vide -> Vide
  |Noeud(x, g, d) -> if k < x then Noeud(x, supprime k g, d)
      else if k > x then Noeud(x, g, supprime k d)
      else if g = Vide then d
      else let u, gauche = enleverDroite g in 
          Noeud(u, gauche, d);;

(* TAS *)

type 'a tas = {mutable n : int ; mutable donnees : 'a array };;

let maximum t = t.donnees.(0);;

let filsGauche p = 2 * p + 1;;
let filsGauche p = 2 * p + 2;;

let valeurFilsGauche arbre m =
  if 2 * m + 1 >= arbre.n then failwith "le noeud fourni en entrée est une feuille"
  else arbre.donnees.(2 * m + 1);;

let estRacine n = (n = 0);;

let echange t m1 m2 =
  let u = t.donnees in
  let x1 = u.(m1) and x2 = u.(m2) in
    u.(m1) <- x2;
    u.(m2) <- x1;;

let estVideSousArbre p arbre = (p >= arbre.n);;

let echange t m n = 
  let x = t.(m) in 
    t.(m) <- t.(n);
    t.(n) <- x;;

let rec montee t n = 
  if n > 1 then begin
    let m = n / 2 in
      if t.(m) < t.(n) then begin
        echange t m n;
        montee t m;
      end;
  end;;

let ajout t x = 
  let n = t.(0) in 
    t.(n + 1) <- x;
    montee t (n + 1);
    t.(0) <- n + 1;;

let make_tas t = 
  let n = t.(0) in 
    for i = 1 to n do 
      montee t i
    done;;

let rec descente t k n = 
  if (2 * k < n) && (t.(k) < t.(2 * k)) || (2 * k + 1 < n) && (t.(k) < t.(2*k + 1)) then 
    if (2 * k + 1 < n) && (t.(2 * k) < t.(2 * k + 1)) then begin
      echange t k (2 * k + 1);
      descente t (2 * k + 1) n;
    end
    else begin 
      echange t k (2 * k);
      descente t (2 * k) n;
    end;;

let suppression t k = 
  let n = t.(0) in 
    echange t k n;
    descente t k n;
    t.(0) <- n - 1;;












