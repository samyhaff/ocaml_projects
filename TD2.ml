let rec euclide u v = match v with
  |0 -> u
  |a -> euclide v (u mod v);;

(* int -> int *)

let rec s n =
  let rec sAux n acc = match n with
    |0 -> acc
    |a -> sAux (n - 1) (acc + n)
  in sAux n 0;;

let rec hanoi n i j k =
  if n = 1 then print_string ("\n deplacer le disque de la tour"^" "^string_of_int(i)^" "^"a la tour"^" "^string_of_int(k)^" ")
  else
    begin
      hanoi (n - 1) i k j;
      print_string ("\n deplacer le disque de la tour"^" "^string_of_int(i)^" "^"a la tour"^" "^string_of_int(k)^" ");
      hanoi (n - 1) k i j;
    end;;

let rec itere f x0 n = match n with
  |0 -> x0
  |n -> f (itere f x0 (n - 1));;

let rec itereBis f x0 n = match n with
  |0 -> x0
  |a -> itereBis f (f x0) (n - 1);;

let rec floyd1 f x0 =
  let rec floydAux f x y = match x, y with
    |a, b when a = b -> a
    |a, b -> f (f a) (f (f b))
  in floydAux f (x0) (f x0);;

let rec floyd2 f x0 =
  let rec floydAux f x y i = match x, y with
    |a, b when a = b -> i
    |a, b -> f (f a) (f (f b)) (i + 1)
  in floydAux f (x0) (f x0) 1;;

let periode f x0 =
  let i = floyd2 f x0 in floyd2 f (itere f x0 i);;
