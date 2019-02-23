let rec fact n =
  let rec factAux n acc = match n with
    |0 -> acc
    |n -> factAux (n - 1) (n * acc)
  in factAux n 1

let fib n =
  let rec fibAux n a b = match n with
    | 0 -> a
    | n -> fib (n - 1) (a + b)
  in fibAux n 0 1
