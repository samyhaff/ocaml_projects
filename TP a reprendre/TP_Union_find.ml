type partition = (int * int) array ;;let init_part n = 
let (v:partition) = Array.make n (0,0) in
for i = 0 to (n-1) do
v.(i) <- (i,0)
done;
v;;let rec find (p:partition) i = match fst p.(i) with  | j when i = j -> j  | j            -> let k = find p j in p.(i) <- (k, snd p.(i)) ; k ;;let union p i j =  let ci = find p i and cj = find p j in  match snd p.(ci), snd p.(cj) with    | _ when ci = cj      -> ()    | ri, rj when ri < rj -> p.(ci) <- (cj, snd p.(ci))    | ri, rj when ri > rj -> p.(cj) <- (ci, snd p.(cj))    | ri, rj              -> p.(ci) <- (cj, snd p.(ci)) ; p.(cj) <- (cj, snd p.(cj) + 1) ;;(* calcul des composantes connexes *)type voisin = int list ;;type graphe = voisin array ;;let composantes (g:graphe) =  let n = Array.length g in  let p = init_part n in  let rec aux i = function    | []   -> ()    | j::q -> union p i j ; aux i q in  for i = 0 to n-1 do aux i g.(i) done ;  (p:partition) ;;(* Evaluation de la hauteur maximale *) open Random ;;
let (p:partition) = init_part 1000000 ;;for k = 1 to 1000000 do   let i = int 1000000 and j = int 1000000  in union p i j done ;;let hauteur (p:partition) i =  let rec aux h = function    | j when fst p.(j) = j -> h    | j                    -> aux (h+1) (fst p.(j))  in aux 0 i ;;let maxhauteur (p:partition) =  let n = Array.length p in  let rec aux acc = function    | j when j = n -> acc    | j            -> aux (max acc (hauteur p j)) (j+1)  in aux 0 0 ;;maxhauteur p ;;





