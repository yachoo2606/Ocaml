type 'a btree = 
  Leaf of 'a
| Empty
| Node of 'a btree * 'a btree

let print_tree d string_of_leaf = 
  let rec f d =
    match d with
      Empty -> "Empty"
    | Leaf i -> "Leaf"^(string_of_leaf i)
    | Node(x,y) -> "Node("^f x^", "^f y ^ ")"
in Printf.printf "%s" (f d);;

let sum convert tree = 
  let rec f d sum=
    match d with
      Empty -> sum + 0
    | Leaf i -> sum + convert i
    | Node(x,y) -> sum + f x sum + f y sum
in f tree 0;;

sum int_of_string (Node(Leaf "10", Node(Leaf "20", Empty)));;

let rec map convert tree = 
    match tree with
      Empty -> Empty
    | Leaf i -> Leaf(convert i)
    | Node(x,y) -> Node(map convert x,map convert y);;

map int_of_string (Node(Leaf "10", Node(Leaf "20", Empty)));;

let comp x y  =
  if x = y then true
else false

let rec mount rootTree subTree place comparator = 
  match rootTree with
  | Empty -> Empty
  | Leaf y when (comparator place y ) -> subTree
  | Leaf _ -> rootTree
| Node (left, right) -> Node(mount left subTree place comparator, mount right subTree place comparator);;

let sosna = (Node(Leaf 10, Node(Leaf 20, Node(Leaf 20, Node(Leaf 20, Empty)))));;
let galez = (Node(Leaf 2, Node(Empty, Leaf 3)));;

mount sosna galez 20 comp;;

let rec fold_left operator acc tree =
  match tree with
  | Leaf x -> operator acc x
  | Empty -> acc
  | Node (l, r) ->
let acc = fold_left operator acc l  in
fold_left operator acc r;;
  
let sum = fold_left ( + ) 0 sosna;;
let mul = fold_left ( * ) 1 sosna;;


let rec fold_right f tree acc =
  match tree with
  | Leaf x -> f x acc
  | Empty -> acc
  | Node (l, r) ->
      let acc = fold_right f r acc in
fold_right f l acc;;

sosna;;

let sum = fold_right ( + ) sosna 0;;
let mul = fold_right ( * ) sosna 1;;

let sub = fold_right ( - ) sosna 0;;
let sub = fold_left ( - ) 0 sosna;;


let rec insert tree data =
  match tree with
  | Leaf _ -> Node (Leaf data, Empty)
  | Empty -> Leaf data
  | Node (left, right) ->
    match insert left data with
    | new_left -> Node (new_left, right);;

let addedTree = insert galez 2;;


