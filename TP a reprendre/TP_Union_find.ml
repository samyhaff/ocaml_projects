type partition = (int * int) array ;;
let (v:partition) = Array.make n (0,0) in
for i = 0 to (n-1) do
v.(i) <- (i,0)
done;
v;;





