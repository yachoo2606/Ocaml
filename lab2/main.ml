(*
  rlwp ocaml   
*)

let mnozenieTablicy l = match l with
  [_;_;_] -> List.map (fun x -> x * 3) l
| _ -> [];;

let wysweitl l = print_string (String.cat (String.concat " " (List.map string_of_int l)) "\n");;


wysweitl (mnozenieTablicy [1;2;3]);;

let test = [1;2;3]

let rec append lis1 lis2 = 
  match lis1 with
  [] ->lis2
  | hd :: tail -> hd :: (append tail lis2)
;;

append test [1];;


let range a b =
  let rec f x y accum = 
    if x>y then 
      accum
    else
      f x (y-1) (y::accum)
    in f a b [];;

range 1 10;;

let append a b =
  let rec reverse l accum =
    match l with
      [] -> accum
      | h::tail -> reverse tail (h::accum)
  in reverse (reverse a []) b ;;

append [2;2;3] [1;2;3;4];;

let append l1 l2 = List.fold_right(fun x y -> x::y) l1 l2;;
append [2;2;3] [1;2;3;4];;

let reverse2 list = List.fold_right(fun x y -> y @ [x] ) list [];; (*przepenia stos*)
reverse2 [1;2;3]

(* https://sookocheff.com/post/ocaml/starting-a-new-ocaml-project-using-dune-and-visual-studio-code/ *)