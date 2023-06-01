
(*zaimplementuj funkcje butlust ktora pobiera argument l: 'a List i zwraca liste zawierajca wszystkie elementy l oprocz ostatniego elementu tej listy *)

let rec butlast l = match l with
  []  -> []
| [x] -> []
| elem :: tail -> elem :: butlast tail;;


butlast [1;2];;

butlast [1;2;3];;

(* Oblicz dlugosc listy za pomoca fold_left fold_right *)

let len_fl l = List.fold_left (fun x _ -> x + 1 ) 0 l;;

let len_rl l = List.fold_right (fun _ y -> y + 1 ) l 0;;

(* Zaimplementuj funkcje, ktora zwraca kolejnosc elementow na liscie *)

let rec reverse l = match l with
  [] -> []
| [x] -> [x]
| elem :: tail -> (reverse tail) @ [elem];;

reverse [1;2;2;23;4;54;3];;

(* lepsze rozwizanie bo tail recursion....... *)

let reverse list =
  let rec hellper accum l = match l with
    []-> accum
    |head::tail -> hellper(head::accum) tail
in hellper [] list;;

reverse [1;2;2;23;4;54;3];;

(* z foldem to samo.... *)

let reverse list = List.fold_left(fun x y -> y::x) [] list;; (*lepsza wersja bo tail rec*)
reverse [1;2;2;23;4;54;3];;

let reverse2 list = List.fold_right(fun x y -> y @ [x] ) list [];; (*przepenia stos*)
reverse2 [1;2;2;23;4;54;3];;

let rec rev l =
  let rec rev2 acc = function
    [] -> acc
    | h::t -> rev2 (h::acc) t
in rev2 [] l;;

rev [1;2;2;23;4;54;3];;


