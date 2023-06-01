(*
  
  ocamlc prog.ml -o prog
  
 *)

Printf.printf "%i\n" 10;;
Printf.printf "%f\n" 10.;;
Printf.printf "%s\n" "Ala";;

open Printf;;
printf "%s" ("ala " ^ "i kot.");;

1 + match [1;2;3] with
  [] -> 0
| h::t -> printf "%i\n" h; h;;

(* wariant 1 *)

let rec len = function
  [] -> 0
| h::t -> 1 + len t;;

(* wariant 2 - tail recursion *)

let len list =
  let rec f n = function
      [] -> n
    | _::t -> f (n+1) t
  in 
  f 0 list;;

printf "%i" (len [1; 2; 3; 4]);;
printf "%i" (len ["ala"; "kot"]);;

let print_int_list l =
  let rec string_of_list l = 
    match l with 
      [] -> ""
    | h::tail -> string_of_int h^";"^string_of_list tail
  in
  Printf.printf "[%s]\n" (string_of_list l);;

print_int_list [1; 2; 3; 4];;

let range a b = 
  let rec f x y accum = 
    if x > y then 
      accum
    else 
      f x (y-1) (y::accum)
  in 
  f a b [];;
  
print_int_list (range 1 10);;

let print_list cf l =
  let rec string_of_list l = 
    match l with 
      [] -> ""
    | h::tail -> cf h^";"^string_of_list tail
  in
  Printf.printf "[%s]\n" (string_of_list l);;

print_list string_of_int (range 1 10);;
print_list (fun x -> x) ["ala"; "i kot"];;

(1,"ala",10.);;

type rec_t = {a:int; b:float};;

{a=1; b=2};;

type floatstringpair = float * string;;

let f (x : floatstring) = 1;;
f (12,30);;

type foo =
    Nic
  | Liczba of int
  | Para of floatstringpair
  | Tekst of string;;

let x = Para (3.,"ala");;
let y = Nic;;
let z = Tekst (" i kot.");;

let f x:foo = x;;

f x;;
f y;;
f z;;
f (10.,"ala") (* wrong type! *)
f (Para (10.,"ala"));;

let Para(a,b) = x;; 
a+.100.;;

let l = [x; y; z];;

match l with
  [] -> printf "%s" "lista pusta"
| h::t ->
    match h with 
      Para (a,b) -> printf "%f" a
    | Nic -> printf "%s" "sorry, nic"
    | _ -> ();;

(* dab

  /\
 2 /\
  4 5  

*)

type btree = 
    Leaf of int
  | Node of btree * btree;;

let lisc = Leaf 1;;
let dab = Node ((Leaf 2), Node (Leaf 4, Leaf 5));;

(* dąb

  /\
 / /\
2 4 5  

sosna

   /\
10.  \
     2.5  

*)

type 'a btree = 
    Leaf of 'a
  | Nodee of 'a btree
  | Node of 'a btree * 'a btree;;

let dab = Node (Leaf "ala", Node (Leaf "hello", Leaf "kot"));;
let sosna = Node (Leaf 10., Nodee (Leaf 2.5));;

lisc;;
dab;;
sosna;;

(* jodła

   /\
10. /
   2.5  

 *)

(* Definicja drzewa niezrównoważonego, która rozróżnia lewe i prawe poddrzewo *)

type 'a btree = 
  Leaf of 'a
| Empty
| Node of 'a btree * 'a btree;;

let sosna = Node ((Leaf 10.), Node (Empty, Leaf 2.5));;
let jodla = Node ((Leaf 10.), Node (Leaf 2.5, Empty));;

let d = Node(Leaf 1, Leaf 2);;
let Node (l1,l2) = d;;

printf "%i" (match lisc with Leaf x->x);;
Printf.printf "%s" (match lisc with Leaf x-> "Leaf "^(string_of_int x));;

let print_tree cf d = 
  let rec string_of_tree d =
    ...
  in
  printf "%s\n" (string_of_tree cf d);;

print_tree string_of_float sosna;;
print_tree (fun x -> x) dab
