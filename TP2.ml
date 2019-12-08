type expr = Var of char
            |Const of float
            |Exp of expr
            |Somme of expr * expr
            |Produit of expr * expr;;

let rec calcule expr = match expr with
  |Var(e) -> failwith "une variable apparait dans l'expresion"
  |Const(e) -> e
  |Exp(e) -> exp (calcule e)
  |Somme(e1, e2) -> calcule(e1) +. calcule(e2)
  |Produit(e1, e2) -> calcule(e1) *. calcule(e2);;

let rec evalue expr fonction = match expr with
  |Var(e) -> fonction e
  |Const(e) -> e
  |Exp(e) -> exp (calcule e)
  |Somme(e1, e2) -> calcule(e1) +. calcule(e2)
  |Produit(e1, e2) -> calcule(e1) *. calcule(e2);;

let rec derive v expr = match expr with
  |Var(e) -> if e = v then Const(1.) else Const(0.)
  |Const(e) -> Const(0.)
  |Exp(e) -> Produit(derive v e, Exp(e))
  |Somme(e1, e2) -> Somme(derive v e1, derive v e2)
  |Produit(e1, e2) -> Somme(Produit(derive v e1, e2), Produit(derive v e2, e1));;

derive 'x' (Exp (Somme (Var 'x', Const 1.)));;

let rec simplifie expr = match expr with
  |Var(e) -> Var(e)
  |Const(e) -> Const(e)
  |Exp(e) when e <> Const(0.) -> Exp(e)
  |Exp(Const(0.)) -> Const(1.)
  |Somme(e, Const(0.)) -> simplifie e
  |Somme(Const(0.), e) -> simplifie e
  |Produit(e, Const(1.)) -> simplifie e
  |Produit(Const(1.), e) -> simplifie e;;

let rec simplifieTotale expr =
  let exprSimplifiee = ref expr in
  while simplifie !exprSimplifiee <> !exprSimplifiee do
    exprSimplifiee := simplifie !exprSimplifiee
  done;
  !exprSimplifiee;;

type expr = C of string | U of string * expr | B of string * expr * expr;;

let rec ecritpost expr = match expr with
  |C(c) -> c
  |U(c, e) -> ecritpost(e)^c
  |B(c, e1, e2) -> ecritpost(e1)^ecritpost(e2)^c;;

let arbre_de_liste l =
  let rec aux l pile = match (l, pile) with
    |[], [t] -> t (* la pile contient l'arbre t en entier *)
    |(0, x)::l1, p -> aux l1 C(x)::p
    |(1, x)::l2, t::p -> aux l1 U(x, t)::p
    |(2, x)::l2, t1::t2::p -> aux l1 B(x, t1, t2)::p
    |_ -> failwith "erreur"
  in aux l [];;

let parse chaine =
