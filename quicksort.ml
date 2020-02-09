let echange t i j =
  let a = t.(i) in
  t.(i) <- t.(j) in
  t.(j) <- t.(i);;

let rec decoupe t p i j = match j with
  |j when j < i -> j
  |j when t.(i) < p -> decoupe p t (i + 1) j
  |j -> echange t i j; decoupe t p i (j- 1);;
