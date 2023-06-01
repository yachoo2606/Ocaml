type foo = 
  NIC
| Para of int * int
| Tekts of string;;

let x = Para(3,4);;
let y = NIC;;
let z = Tekts("ALA");;

let f x:foo = x;;

f NIC;;
f (Para(100,100));;

let x = Para(100,200);;
let Para(a,b) = x;;
b+1;;

z;;
match z with 
  Tekts s -> s^"Kot"
| _ -> "";;


(* DRZEWA *)

type btree = 
  Leaf of int
| Node of btree * btree;;

let dab = Node(Node(Leaf 1, Leaf 2), Leaf 3);;

let lisc = Leaf 1;;

type btree = 
  Leaf of int
| Nodee of btree
| Node of btree * btree;;

let sosna = Node(Leaf 10, Nodee(Leaf 20));;

type btree = 
  Leaf of int
| Empty
| Node of btree * btree;;

let sosna = Node(Leaf 10, Node(Empty, Leaf 20))
let sosna = Node(Leaf 10, Node(Leaf 20, Empty))

type 'a btree =
Leaf of 'a
| Empty
| Node of 'a btree * 'a btree;;

let sosna = Node(Leaf 10, Node(Leaf 20, Node(Leaf 20, Leaf 55)));;

let Node(d1,d2) = sosna;;


let rec printTree cf tree = match tree with
| Empty -> cf^""
| Leaf(lisc) -> cf^"leaf: "^(string_of_int lisc)
| Node(d1,d2) -> cf ^ "Node: " ^ printTree cf d1 ^ " " ^ printTree cf d2;;

printTree "" sosna;;

let printList cf l = 
  let rec str_of_list l =
    match l with 
     [] ->""
    | h::tail -> (cf h)^";"^str_of_list tail
  in
    Printf.printf "%s\n" ("["^str_of_list l^"]");;

printList string_of_int [1;2;3;4;5];;