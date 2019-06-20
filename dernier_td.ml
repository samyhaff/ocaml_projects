open Stack;;
open List;;
open Queue;;
type 'a arbre = Vide | Noeud of 'a * 'a arbre * 'a arbre;;

(* EXERCICE 1 *)

let prefixe arbre = match arbre with
  |Vide -> []
  |Noeud(a, g, d) ->
    let liste = ref [a] in
    let s = create() in
    push d s;
    push g s;

    while not (is_empty s)
    do
      let u = pop s in
      match u with
      |Vide -> ()
      |Noeud(a, g, d) ->
        l := a::!l;
        push d s;
        push g s;
    done;
    rev l;;

let parcoursLargeur arbre =
  let file = Queue.new() in add arbre file
  let rec auxLarg acc =
    if Queue.is_empty file then acc
    else match (Queue.take file) with
      |Vide -> auxLarg acc
      |Noeud(x, g, d) ->
        add g file;
        add d file;
        auxLarg x::acc;;
in  List.rev (auxLarg []);;

(* EXERCICE 3 *)

let fd i =
  2 * i + 2

let fg i =
  2 * i + 1

let echanger i j tab =
  let k = tab.(i) in
  let tab.(i) = tab.(j) in
  let tab.(j) = k;;
