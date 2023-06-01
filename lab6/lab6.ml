1. Moduł Lista
  
-------------------------------------------------------
# Makefile

all: pr_list

lista.cmi: lista.mli
    ocamlc -c lista.mli

lista.cmo: lista.ml lista.cmi
    ocamlc -c lista.ml
    
pr_list: lista.cmi lista.cmo
    ocamlc -o pr_list lista.cmo pr_list.ml

clean: 
    rm -r *.cmo *.cmi pr_list

-------------------------------------------------------  
(* lista.mli - interface file of module Lista *)

val print : ('a -> string) -> 'a list -> unit
-------------------------------------------------------  
(* lista.ml - interface implementation file *)

let print cf l = 
  let rec str_of_list l =
    match l with 
      [] -> ""
    | h::tail -> cf h^";"^str_of_list tail
  in
  Printf.printf "%s\n" ("["^str_of_list l^"]");;
-------------------------------------------------------  
(* pr_list.ml - application that uses module Lista *)

Lista.print (string_of_int) [1;2;3];;
Lista.print (fun x->x) ["ala"; "kot"];;

(* lub używając open *)

open Lista;;
print (string_of_int) [1;2;3];;
print (fun x->x) ["ala"; "kot"];;
-------------------------------------------------------
     
2. Moduł Inttree

(* Makefile *)

...

(* inttree.mli *)

type btree = Leaf of int | Empty | Node of btree * btree
    
(*
  Zamiast definicji typu w pliku interefejsowym wystarczy napisać

  type btree 
  
  ale wtedy trzeba zdefiniować funkcję construct_tree
 *)
    
val construct_tree -> int list -> btree 
val print_int_tree : btree -> unit
val sum : btree -> int
val map : (int -> int) -> btree -> btree
val fold_int_left : (int -> int -> int) -> int -> btree -> int
val fold_left : ('a -> int -> 'a) -> 'a -> btree -> 'a
val fold_right : (int -> 'a -> 'a) -> btree -> 'a -> 'a
val mount : btree -> btree -> int -> btree

          /\                      /\
mount    1 /\     /\     2   =   1 /\
          2 2    4  5             /\ 2
                                 4  5
(* inttree.ml *)

  ...
     
(* test_inttree.ml *)

open Inttree;;

let dab1 = Node (Leaf 1, Node (Leaf 2, Leaf 3));;
let dab2 = construct_tree [1;2;3];;

print_int_tree dab1;;
print_int_tree dab2;;
  
3. Moduł Tree
  
(*  Makefile *)
  
  ...
  
(* tree.mli *)

type 'a btree = 
    Leaf of 'a 
  | Empty 
  | Node of 'a btree * 'a btree;;

val contruct_tree: 'a list -> 'b btree
val print_tree: 'a btree -> ('a -> string) -> unit
val map: ('a -> 'b) -> 'a btree -> 'b btree
val sum: ('a -> 'a -> 'a) -> 'a -> 'a btree -> 'a
val fold_left: ('a -> 'b -> 'a) -> 'a -> 'b btree -> 'a
val fold_right: ('b -> 'a -> 'a) -> 'b btree -> 'a -> 'a
val delete ...
val insert ...
val mount: 'a btree -> 'a btree -> 'a -> ('a -> 'a -> bool) -> 'a btree 

    (*
      Semantyka:

      let l = [1;2;4]
      
      let d = construct_tree l =   /\
                                  /\ 4
                                 1 2

      map string_of_int d = /\
                           /\ "4"
                        "1" "2"

      sum (+) 0 d = 7

      fold_left i fold_right podobnie do List.fold_left i List.fold_right
      
      let k = delete d 2 =  /\
                           /  4
                          1

      let g = insert d 3 =   / \
                            /\ /\
                           1 2 3 4
      
      mount k d 2 (=) =  /\
                        /\ 4
                       1 /\
                        /  4
                       1
     *)
  
(* tree.ml *)

type 'a btree = 
  Leaf of 'a 
| Empty 
| Node of 'a btree * 'a btree;;

let print_tree d string_of_leaf =
  let rec f d =
    match d with
      Leaf i    -> "Leaf " ^ string_of_leaf i 
    | Empty     -> "Empty"
    | Node(x,y) -> "Node (" ^ f x ^ ", " ^ f y ^ ")"
in
  printf "%s" (f d) ;; 

...

(* test_tree.ml *)

open Tree;;

let d = Node(Leaf 1, Empty);;
print_tree d string_of_int;;  

(* Notacja prefiksowa *)

let f g x y = g x y;;

f (+) 1 3;;
f ( * ) 2 3;;

(+) 1 3;;

Tree.sum (+) 0 d;;

