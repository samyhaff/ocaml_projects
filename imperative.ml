let imprime_chiffre () =
  for i = 0 to 9 do
    print_int i
  done;
  print_newline ();;

let p = [|3; 2; 1|];;
let q = make_vect 4 2;;
(*  vect_lengh q;; *)
(*  q.(0) <- 1 *)

let imprime_monome coeff degre =
  if degre = 0 then print_int coeff else
  if coeff <> 0 then
    begin
      print_string " + ";
      if coeff <> 1 then print_int coeff;
      print_string "x";
      if degre <> 1 then
        begin print_string "^"; print_int degre end
    end;;

let imprime_polynome p =
  for i = 0 to vect_lengh p - 1 do imprime_monome p.(i) i done;;

let compteur = ref 0;
(* !compteur;; *)
compteur := 2;;
compteur := !compteur + 1

let incremente c = c := !c + 1;;

let fact n =
  begin
    let accu = ref 1 in
    for i = 1 to n do accu := i * !accu done;
    !accu
  end;;
