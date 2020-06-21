type expr = Var of char
          |Const of float 
          |Exp of expr 
          |Somme of expr * expr
          |Produit of expr * expr

let rec calcule e = match e with 
  |Var(_) -> failwith "l'expression contient une variable"
  |Const(x) -> x
  |Exp(x) -> exp (calcule x)
  |Somme(e1, e2) -> (calcule e1) +. (calcule e2)
  |Produit(e1, e2) -> (calcule e1) *. (calcule e2);;

let rec evalue e f = match e with
  |Const(x) -> x
  |Exp(x) -> exp (evalue x f)
  |Somme(e1, e2) -> (evalue e1 f) +. (evalue e2 f)
  |Produit(e1, e2) -> (evalue e1 f) *. (evalue e2 f)
  |Var(x) -> f x;;

let rec derive x e = match e with 
  |Const(x) -> Const(0.)
  |Var(y) when y = x -> Const(1.)
  |Var(y) -> Const(0.)
  |Exp(y) -> Produit(derive x y, Exp(y))
  |Somme(e1, e2) -> Somme(derive x e1, derive x e2)
  |Produit(e1, e2) -> Somme(Produit(e1, derive x e2), Produit(e2, derive x e1));;

derive 'x' (Exp(Somme(Var 'x', Const 1.)));;

let rec simplifie e = match e with 
  |Somme(Const 0., x) -> e
  |Somme(x, Const 0.) -> e
  |Produit(Const 1., x) -> x
  |Produit(x, Const 1.) -> x
  |Exp(Const 0.) -> Const 1.;;

type expr = C of string | U of string * expr | B of string * expr * expr;;

let rec ecritpost e = match e with 
  |C s -> print_string s
  |U(s, e1) -> ecritpost e1; print_string s 
  |B(s, e1, e2) -> ecritpost e1; ecritpost e2; print_string s;;

let arbre_de_liste l = 
  let p = ref [] in 
  let rec aux l = match l with 
    |[] -> ()
    |(0, s)::q -> p := (C s)::(!p); aux q
    |(1, s)::q -> let t = List.hd !p and 
      pp = List.tl !p in 
          p := U(s, t)::pp; aux q
    |(2, s)::q -> let t1 = List.hd !p and
      t2 = List.hd (List.tl !p) and
      pp = List.tl (List.tl !p) in
          p:= B(s, t1, t2)::pp; aux q
    |_ -> failwith "erreur"
  in 
    aux l;
    match !p with 
      |[x] -> x
      |_ -> failwith "erreur";;

let rec parse s = 
  let liste = ref [] in 
  let n = String.length s in 
    for i = 0 to n - 1 do 
      let c = s.[i] in 
        if c = 'B' then liste := (!liste)@[(2, String.make 1 s.[i + 1])]
        else if c = 'U' then liste := (!liste)@[(1, Sring.make 1 s.[i + 1])]
        else if c = 'C' then liste := (!liste)@[(0, String.make 1 s.[i + 1])]
    done;
    !liste;;

parse "C1CxB+";;

let arbre_de_chaine s = 
  arbre_de_liste (parse s);;
































