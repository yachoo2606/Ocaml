(*
  rlwp ocaml   
*)


(* "nowe" typy danych *)

(* ocamlc prog.ml -o program  *) (*compilowanie programu*)

Printf.printf "Wynik = %i\n" 10;;
Printf.printf "Wynik = %f\n" 10.2121;;
Printf.printf "Wynik = %s\n" "No siema";;
(* 
i-integer
f-float
s-string    
*)

(* wysweitlanie naszych struktur dancyh *)

let print_int_list l = 
  let rec listToString = function
    [] -> ""
    | h::tail -> string_of_int h ^";"^listToString tail
  in Printf.printf "[%s]\n" (listToString l);;
  
print_int_list [1;2;3;4;5];;

let print_int_list cv l = 
  let rec listToString = function
    [] -> ""
    | h::tail -> cv h ^";"^listToString tail
  in Printf.printf "[%s]\n" (listToString l);;

print_int_list string_of_int [1;2;3];;
print_int_list (fun x->x) ["ala";"ale"];;

type rec_t = {a:int; b:float};;

{a=1;b=2.};;

type floatStringpair = float * string;;

let f (x: floatStringpair) = 1;;

f (10. ,"cos")

type foo = 
  Nic
| Liczba of int
| NaszTyp of floatStringpair;;

