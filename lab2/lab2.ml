let x = 1;;
let f x y = x+y;;

let r = ref 1;;

r:=2;;
!r + 3;;

let u = r:=10;;
u;;

r:=3; r:=4;;

!r;;

r:=3 +(!r);;
!r;;

r := "Ala";;

let r = ref (fun a b -> a ^ b);;

!r "ala" "kot";;

let f x = 
  let r = fun a -> a-x in 
  r;;
  
f 1 2;;  
  
let f1 = f 1;;
let f2 = f 2;;  

f1 10;;
f2 10;;
  
let f x = 
  let r = ref (fun a -> a-x) in 
  r;;
  
let f1 = f 1;;
let f2 = f 2;;
  
!f1 10;;
!f2 10;;

let g x a = 
  x := !x + a;
  !x;;

let g x a = 
  x := !x + a  (* brak przecinka => program poprawnie typowany, ale co on robi? *)
  !x;;

let y = ref 0;;

g y 1;;
g y 1;;  
  
let g x = fun y -> x-y;;

g 2 3;;
(g 2) 3;;
  
let g x = fun y -> x y;;
 
let f x = x+1;; 

g f 10;;
g f;;

let ff = g f;;

ff 10;;

[1; 2; 3];;
[1; "ala"];; (* wrong *)

let l = 1::2::3::[];;
let k = (1::(2::(3::[])));;

l @ k;;

["ala"; "kot"];;

let para = ('a', l);;
('a', l, 1000);;

[para; ('b',[1])];;

let double x = x*2;;
List.map double [1;2;3];;

let f l =
  let double x = x*2 in
  List.map double l;;
  
f [2;3;4;5];;  
 
let l = [5;6];;
let ll = [10;11];; 

f l@ll;;
(f l)@ll;;
  
let l = [10;2;11];;
  
match l with
  [] -> 0
| [a] -> a+1
| [a;b] -> a+b
| _ -> -1;;
 
match l with
  [] -> 0
| [a] -> a+1
| a::b::g -> a+b;;

(*
Zdefiniuj funkcję 1-argumentową, która przyjmuje jako argument listę l i zwraca listę zawierającą elementy listy l przemnożone przez 3, pod warunkiem, że l ma co najmniej 3 elementy. W przeciwnym razie zwróć listę pustą.
*)

let f l =
  let mpt l =
     List.map (fun x -> x*3) l
  in
  match l with 
    [] -> []
  | [a] -> []
  | [a;b] -> []
  | _ -> mpt l;;
   
 
let f l =
  let mpt l =
     List.map (fun x -> x*3) l
  in
  match l with 
    a::b::c::g -> mpt l
  | _ -> [];;
  
 
 f [1];;
 f [1;2;3];;
 
(* Zdefniuj funkcję append *)  
