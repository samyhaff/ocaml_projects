let rec fact n =
  if n = 0 then 1 else n * fact(n - 1);;

let rec factorielle n = funnction
| 0 -> 1
| n -> n * factorielle (n - 1);;

let rec enumeration n =
  if n <= 0 then () else
    begin
      (* changer l'ordre des lignes <-> changer ordre d'énumeration *)
      print_int n;
      print_string " ";
      enumeration(n - 1)
    end;;

let next x = x + 1;;
trace "next"
trace "enumeration"

let rec ́epelle_envers_aux s i =
  if i >= 0 then
    begin
      print_char s.[i]; print_char ' ';
        epelle_envers_aux s (i - 1)
    end;;

let epelle_envers s = epelle_envers_aux s (string_length s - 1);;

let rec palindrome s =
  let longueur = string_length s in
  if longueur <= 1 then true else
  if s.[0] = s.[longueur - 1]
  then palindrome (sub_string s 1 (longueur - 2))
  else false;;

(* palindrome si de longueur <=1 ou si (2 termes extremes identiques et sous chaine palindrome *)
let rec palindrome s =
    let longueur = string_length s in
    (longueur <= 1) ||
      (s.[0] = s.[longueur - 1]) &&
      (palindrome (sub_string s 1 (longueur - 2)));;

let palindrome s =
    let rec palin i j =
        i >= j || s.[i] = s.[j] && palin (i + 1) (j - 1) in
    palin 0 (string_length s - 1);;
