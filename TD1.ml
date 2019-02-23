let rec fact n =
  let rec factAux n acc = match n with
    |0 -> acc
    |n -> factAux (n - 1) (n * acc)
  in factAux n 1;;

let fib n =
  let rec fibAux n a b = match n with
    |0 -> a
    |n -> fibAux (n - 1) (a + b)
  in fibAux n 0 1;;

let nombreChiffres n =
  let rec nombreChiffresAux n b acc = match n with
    |0 -> acc
    |n -> nombreChiffresAux (n / b) (acc + 1)
  in nombreChiffresAux n b 0;;

let implication a b =
  not b || a;;

let max4 x y z t = max max x y z t;;

let decurrifier f =
  fun (a, b) - > f a b;;
