let conflit i j = 
  ((fst i) <= (snd j)) && ((snd i) >= (fst j));;

let construit_graphe segments = 
  let n = Array.length segments in 
  let g = Array.make n [] in 
    for i = 0 to n - 1 do 
      for j = i + 1 to n - 1 do
        if (conflit segments.(i) segments.(j)) then 
          begin
            g.(i) <- j::g.(i);
            g.(j) <- i::g.(j);
          end;
      done;
    done;
    g;;

let plus_petit_absent l = 
  let rec aux l x = 
    if appartient l x then x 
    else aux l (x + 1) 
  in aux l 0;;

let couleurs_voisins aretes couleurs i = 
  let n = Array.length aretes.(i) in 
  let couleurs_voisins = ref [] in 
    for j = 0 to n - 1 do 
      let c = couleur.(aretes.(i).(j)) in 
        if c <> (-1) then couleurs_voisins := c::(!couleurs_voisins)
    done;
    couleurs_voisins;;

let couleur_disponible aretes couleurs i = 
  let couleurs_prises = couleurs_voisins aretes couleurs i in 
    plus_petit_absent couleurs_prises;;

let est_clique aretes xs = 
  let n = Array.length xs in 
  let test = ref true in
    for i = 0 to n - 1 do 
      for j = 0 to n - 1 do 
        if j <> i && (not appartient aretes.(i) xs.(j)) then 
          test := false 
      done;
    done;
    !test;;








