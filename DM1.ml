let bisextile n = match n with
  |a when (n mod 4 = 0 && n mod 100 <> 0) || n mod 400 = 0 -> true
  |_ -> false;;

(* int -> float -> int -> float *)

let operateur u =
  fun n -> u (n + 1) -. u n;;

let rec log2 x =
  let rec log2Aux x k p = match x with
    |0 -> k - 1
    |a when x < 0 -> k - 1
    |x -> log2Aux (x - p) (k + 1) (2 * p)
  in log2Aux x 0 1;;

let rec palindrome s =
  let rec palindromeAux s f l = match s with
    |a when f = l -> true
    |a when not (String.get s f = String.get s l) -> false
    |s -> palindromeAux s (f + 1) (l - 1)
  in palindromeAux s 0 (String.length s - 1);;

let rec f =
  let rec gAux = function
    |0, n -> n
    |m, n -> f ((m - 1), n) + 1
  in function
    |0, n -> n
    |m, n -> gAux ((m - 1), (m + 1));;

let rec egyptienne a b = match a with
  |x when x <= 0 -> 0
  |x when x mod 2 = 0 -> egyptienne (a / 2) (b + b)
  |_ -> (egyptienne (a / 2) (b + b)) + b;;

(*
function multiply(x, y)
  p ← 0
  while x > 0 do
    if x est impair then
      p ← p + y
    x ← x/2
    y ← y + y
  return p
*)

let rec egyptienne2 a b =
  let rec egyptienneAux a b acc = match a with
    |x when x <= 0 -> acc
    |x when x mod 2 <> 0 -> egyptienneAux (a / 2) (b + b) (acc + b)
    |_ -> egyptienneAux (a / 2) (b + b) acc
  in egyptienneAux a b 0;;
