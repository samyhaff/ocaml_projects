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

let rec nombreChiffres n =
  let rec nombreChiffresAux n acc = match n with
    |0 -> acc
    |n -> nombreChiffresAux (n / 10) (acc + 1)
  in nombreChiffresAux n 0;;

let nombreChiffresBase a b =
  1 + int_of_float ( log ( float_of_int a )/. log ( float_of_int b ));;

let decurrifier f =
  fun (a, b) - > f a b;;
