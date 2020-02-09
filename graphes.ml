(* loste d'adjacence *)

type 'a noeud = int;;
type 'a graphe = ('a noeud list) array;;

let nombreNoeuds g = Array.length g;;

let rec enleverElt elt liste = match liste with
  |[] -> []
  |t::q when t = elt -> q
  |t::q -> t::(enleverElt elt q);;

let rec membre elt liste = match liste with
  |[] -> false
  |t::q when t = elt -> true
  |t::q -> membre elt q;;

let presenceArete graphe i j = membre j graphe.(i);;

let ajouteArete graphe i j = 
  if not (presenceArete graphe i j) then 
    graphe.(i) <- j::graphe.(i);; (* cas d'un graph orienté *)

let supprimerArete graphe i j = 
  if presenceArete graphe i j then 
    graphe.(i) <- enleverElt j (graphe.(i));; (* cas d'un graphe orienté *)

(* matrice d'adjacence *)

type 'a noeud = int;;

let nombreNoeuds graphe = Array.length graphe;;

let presenceArete graphe i j = graphe.(i).(j) = 1;;

let ajouteArete graphe i j = graphe.(i).(j) <- 1;;

let supprimeArete graphe i j = graphe.(i).(j) <- 0;;

(* parcours en largeur *)

let bfs g = 
  let n = Array.length g in 
  let vus = Array.make n false in 
  let rec bfsAux g aVisiter acc = match aVisiter with 
    |[] -> acc 
    |t::q -> if not (vus.(t)) then 
          begin
            vus.(t) <- true;
            bfsAux g (q@g.(t)) (acc@[t])
          end
        else
          bfsAux g q acc;
  in bfsAux g [0] [];;

(* parcours en profondeur *) 

let dfs g = 
  let n = Array.length g in 
  let vus = Array.make n false in 
  let rec dfsAux g aVisiter acc = match aVisiter with 
    |[] -> acc
    |t::q -> if not (vus.(t)) then 
          begin
            vus.(t) <- true;
            dfsAux g (g.(t)@q) (acc@[t]) 
          end
        else
          dfsAux g q acc;
  in dfsAux g [0] [];;

let liste_to_mat g = 
  let n = Array.length g in 
  let m = Array.make_matrix n n 0 in 
    for a = 0 to n - 1 do 
      List.do_liste (function b -> m.(a).(b) <- 1) g.(a)
    done;
    m;;

let mat_to_liste m = 
  let n = Array.length m in 
  let g = Array.make n [] in 
    for i = 0 to n - 1 do 
      for j = 0 to n - 1 do 
        if m.(i).(j) = 1 then g.(i) <- j::g.(i)
      done;
    done;
    g;;

let dfs_rec g s = 
  let dejavu = Array.make (Array.length g) false in 
  let rec aux = function 
    |s when dejavu.(s) -> ()
    |s -> dejavu.(s) <- true;
        traitement s;
        do_liste aux g.(s)
  in aux s;;

(* composantes connexes *) 

let liste_composantes g = 
  let dejavu = Array.make (Array.length g) false in 
  let rec dfs lst  = function 
    |s when dejavu.(s) -> lst 
    |s -> dejavu.(s) <- true;
        it_list dfs (s::lst) g.(s) (*it list f a [ b1 ; ... ; bn ] renvoie f (... (f (f a b1) b2) ...) bn ; *)
  and aux comp = function 
    |s when s = Array.length g -> comp 
    |s when dejavu.(s) -> aux comp (s + 1)
    |s -> aux ((dfs [] s)::comp) (s + 1)
  in aux [] 0;;

(* Floyd-Warshall *) 

type poids = Inf | P of int;;

let somme a b = match a, b with 
  |P x, P y -> P (x + y)
  |_, _ -> Inf;;

let minimum a b = match a, b with 
  |P x, P y -> P (min x y)
  |P x, Inf -> P x
  |Inf, P x -> P x 
  |_, _ -> Inf;;

let infStrict a b = match a, b with 
  |P x, P y -> x < y 
  |P x, Inf -> true 
  |Inf, P x -> false 
  |_, _ -> false;;

let floydWarshall w = 
  let n = Array.length w in 
  let m = Array.copy w in 
    for k = 0 to n - 1 do 
      for i = 0 to n - 1 do 
        for j = 0 to n - 1 do 
          m.(i).(j) <- minimum m.(i).(j) (somme m.(i).(k) m.(k).(j))
        done;
      done;
    done;
    m;;

(* Dijkstra *) 


























































