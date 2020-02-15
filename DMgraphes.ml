let verifie g c = 
  let n = Array.length c in
  let condition = ref true in
  let couples = Array.make n false in 
  let i = ref 0 in 
    while !i < n && !condition do
      if c.(!i) <> - 1 then 
        begin 
          if (not g.(!i).(c.(!i))) || (couples.(c.(!i))) then 
            condition := false;
          couples.(c.(!i)) <- true
        end;
      i := !i + 1
    done;
    !condition;;

let cardinal c = 
  let n = Array.length c in 
  let m = ref n in
    for i = 0 to n - 1 do 
      if c.(i) = -1 then 
        m:= !m - 1;
    done;
    !m;;

let degre1 g i =
  let n = Array.length g in
  let deg = ref 0 in 
    for j = 0 to n - 1 do 
      if g.(i).(j) then 
        deg := !deg + 1;
    done;
    !deg;;

let degre2 g i =
  let n = Array.length g in
  let deg = ref 0 in 
    for j = 0 to n - 1 do 
      if g.(j).(i) then 
        deg := !deg + 1;
    done;
    !deg;;

let arete_min_fail g a = 
  let n = Array.length g in 
  let condition = ref false in
  let minimum = ref n + 1 in 
    for i = 0 to n - 1 do 
      for j = 0 to n - 1 do 
        if g.(i).(j) then 
          begin
            condition := true;
            let x = (degre1 g i) + (degre2 g j) in
              if x < !minimum then 
                begin 
                  minimum := x;
                  a.(0) <- i;
                  a.(1) <- j
                end;
          end;
      done;
    done;
    !condition;;

let arete_min g a = 
  let n = Array.length g in 
  let minimum = ref (2 * n + 1) in 
  let degreA = Array.make n 0 in 
  let degreB = Array.make n 0 in
    for i = 0 to n - 1 do 
      for j = 0 to n - 1 do 
        if g.(i).(j) then 
          begin
            degreA.(i) <- degreA.(i) + 1;
            degreB.(i) <- degreB.(i) + 1
          end;
      done;
    done;

    for i = 0 to n - 1 do 
      for j = 0 to n - 1 do 
        let somme = degreA.(i) + degreB.(j) in 
          if somme < !minimum then 
            begin 
              minimum := somme;
              a.(0) <- i;
              a.(1) <- j
            end
      done;
    done;
    !minimum < 2 * n + 1;;

let supprimer g a = 
  let n = Array.length g in 
    for j = 0 to n - 1 do 
      g.(a.(0)).(j) <- false;
      g.(j).(a.(1)) <- false
    done;;

let algo_approche g1 = 
  let g = copy_matrix g1 in 
  let n = Array.length g in 
  let c = Array.make n (-1) in 
  let a = [|0;0|] in 
    while arete_min g a do 
      c.(a.(0)) <- a.(1);
      supprimer g a;
    done;
    c;;

let une_arete g a = 
  let n = Array.length g in 
  let trouve = ref false in 
    for i = 0 to n - 1 do 
      for j = 0 to n - 1 do 
        if g.(i).(j) then 
          begin
            trouve := true;
            a.(0) <- i;
            a.(1) <- j;
          end;
      done;
    done;
    !trouve;;

let rec meilleur_couplage g = 
  let n = Array.length g in 
  let a = [|0;0|] in 
  let c = Array.make n (-1) in 
    if not (une_arete g a) then c 
    else 
      begin 
        let g' = copy_matrix g in 
        let g'' = copy_matrix g in 
          supprimer g'' a;
          let c1 = meilleur_couplage g' in 
          let c2 = meilleur_couplage g'' in 
            if cardinal c1 > cardinal c2 then 
              c1 
            else
              c2


















