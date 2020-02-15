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


