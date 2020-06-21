let somme a b = 
  let n = Array.length a in 
  let c = Array.make_matrix n n false in 
    for i = 0 to n - 1 do 
      for j = 0 to n - 1 do 
        c.(i).(j) <- a.(i).(j) || b.(i).(j)
      done;
    done;
    c;;

let produit a b = 
  let n = Array.length a in 
  let c = Array.make_matrix n n false in 
    for i = 0 to n - 1 do 
      for j = 0 to n - 1 do 
        let x = ref false in 
          for k = 0 to n - 1 do 
            x := (a.(i).(k) && b.(k).(j)) || (!x)
          done;
          c.(i).(j) <- !x
      done;
    done;
    c;;

let accessible a = 
  let n = Array.length a in 
  let id = Array.make_matrix n n false in
    for i = 0 to n - 1 do 
      id.(i).(i) <- true
    done;
    let b = somme a id in 
    let rec aux n = match n with 
      |1 -> b
      |k when k mod 2 = 0 -> let d = aux (k / 2) in produit d d
      |k -> let d = aux (k / 2) in produit b (produit d d)
    in aux n;;















