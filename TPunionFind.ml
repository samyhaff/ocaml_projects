type partition = (int * int) array;;

let init n = 
  let p = Array.make n (0, 0) in 
    for i = 0 to n - 1 do 
      p.(i) <- (i, 0)
    done;
    p;;

let rec find p i = match fst p.(i) with 
  |j when i = j -> j
  |j -> let k = find p j in p.(i) <- (k, snd p.(i)); k;;

let union p i j = 
  let ci = find p i and cj = find p p in 
    match snd p.(ci), snd p.(cj) with 
      |_ -> when ci = cj -> ()
      |ri, rj when ri < rj -> p.(ci) <- (cj, snd p.(ci))
      |ri, rj when ri > rj -> p.(cj) <- (ci, snd p.(cj))
      |ri, rj -> p.(ci) <- (cj, snd p.(ci)); p.(cj) <- (cj, snd p.(cj) + 1);;
