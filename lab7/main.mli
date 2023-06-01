type 'a btree = Leaf of 'a | Empty | Node of 'a btree * 'a btree

val construct_tree: 'a list -> 'b btree
val print_tree: 'a btree -> ('a -> string) -> unit
val map: ('a -> 'b) -> 'a btree -> 'b btree
val sum: ('a -> 'a -> 'a) -> 'a -> 'b btree -> 'a
val fold_left: ('a -> 'b -> 'a) -> 'a -> 'b btree -> 'a
val fold_right: ('b -> 'a -> 'a) -> 'b btree  -> 'a -> 'a
val mount: 'a btree -> 'a btree -> 'a -> ('a -> 'a -> bool) -> 'a btree
val delete: 'a btree -> 'a -> 'a btree
val insert: 'a btree -> 'a -> 'a btree


(* DONE
mount
map
sum 
print_tree
FOLD-l
FOLD-r
INSERT
*)

(* TODO 
    CONSTRUCT
    DELETE
*)