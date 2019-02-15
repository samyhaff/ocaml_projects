let rec euclide u v = match v with
  |0 -> u
  |a -> euclide v (u mod v);;

(* int -> int *)

let rec s n =
  let rec sAux n acc = match n with
    |0 -> acc
    |a -> sAux (n - 1) (acc + n)
  in sAux n 0;;

let rec itere f x0 n = match n with
  |0 -> x0
  |a -> f (itere f x0 (n - 1));;

let rec itere2 f x0 n = match n with
  |0 -> x0
  |a -> itere2 f (f x0) (n - 1);;

let rec floyd1 f x y = match x, y with
  |a, b when a = b -> a
  |a, b -> floyd1 f (f x) (f (f y))
in floyd1 f x0 (f x0);;

let rec floyd2 f x y n = match x, y with
  |a, b when a = b -> n
  |a, b -> floyd2 f (f x) (f (f y)) n + 1
in floyd2 f x0 (f x0) (n + 1);;
