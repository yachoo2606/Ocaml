let double x = 
  x * 2
in
  List.map double [1;2;3];;
  
List.fold_left (fun x y -> x-y) 0 [1;2;3];;

(* ((0-1)-2)-3 = -6  *)

List.fold_left (fun x y -> y-x) 0 [1;2;3];;

(* 3-(2-(1-0)) = 3-1=2 *)

List.fold_right (fun x y -> x-y) [1;2;3] 0;;

(* 1-(2-(3-0)) = 1-(-1) = 2 *)

List.fold_right (fun x y -> y-x) [1;2;3] 0;;

List.fold_left;;
List.fold_right;;

List.fold_left (fun x y -> x^y) "" ["ala"; " i kot"; "."];;

List.fold_right (fun x y -> x^y)["ala"; " i kot"; "."] "";;

type rek_t = {a:int; b:int; c:string};;

let x : rek_t = {a=0; b=1; c="ala"};;
let x = {a=0; b=1; c="ala"};;

x;;

x.a+x.b;;

type obj_t = {a:int; b:int; m:int->int->int};;

let o = {a=1; b=2; m=fun x y -> x+y};;

o.m 1 2;;
o.m o.a o.b;;
o.a=3;; (* false *)
o.a:=3;; (* wrong! *)

type obj_t = {a:int ref; b:int ref; m:int->int->int};;

let o = {a=ref 1; b=ref 2; m=fun x y -> x+y};;
o.a;;
o.m !(o.a) !(o.b);;
o.a:=3;;
o.a;;
!(o.a)+10;;

type obj_t = {a:int ref; b:int ref; m:(int->int->int) ref};;

let o = {a=ref 1; b=ref 2; m=ref (fun x y -> x+y)};;

!(o.m) !(o.a) !(o.b);;
o.m := fun x y -> x-y;;
!(o.m) !(o.a) !(o.b);;

o.m := fun x y -> x*10.;;

(*  1. Zaimplementuj funkcję butlast, która pobiera argument l : 'a List i zwraca listę zawierającą wszystkie elementy l oprócz ostatniego. *)

let rec butlast = function
  [] -> []
| [h] -> []
| h::t -> h::butlast t;;

butlast;;
butlast [1;2;3];;

(* 2. Oblicz długość listy używając fold_right i fold_left. *)

let len_fl l =
  List.fold_left (fun x _ -> x+1) 0 l;;

len_fl [1;2;2];;

let len_fr l = 
  List.fold_right (fun _ y -> y+1) l 0;;
  
len_fr [1;2;2];;

(* 3. Zaimplementuj funkcję, która odwraca kolejność elementów na liście. *)

let rec rev = function 
  [] -> []
| h::t -> (rev t)@[h];; (* ok, but @ is inneficient and stack overflow possible! *) 

rev [1;22;3;4];;

let rev l =
  let rec rev2 l acc = 
     match l with 
       [] -> acc
     | h::t -> rev2 t (h::acc)  
  in
     rev2 l [];;
       
rev [1;2;3;4];;

let rev l =
  List.fold_left (fun x y -> y::x) [] l;;

rev [1;2;3;4];;

let rev l = 
  List.fold_right (fun x y -> y@[x]) l [];;

rev [1;2;3;4];;

let range a b = 
  let rec f x y accum = 
    if x > y then 
      accum
    else 
      f x (y-1) (y::accum)
  in 
  f a b [];;

rev (range 1 1000000);;

let rec append a b =  (* wersja z biblioteki standardowej *)
  match a with
    [] -> b 
  | h::t -> h::(append t b);;

let append a b = 
  let rec rev l acc = 
    match l with
     [] -> acc
   | h::t -> rev t (h::acc)
  in  
    rev (rev a []) b;;

append [1;2] [3;4;5];;

let rev l = 
  List.fold_right (fun x y -> (append y [x])) l [];; 

rev (range 1 100000);;

(* Uwaga: fold_right w Ocaml przepełnia stos, a fold_left nie. *)

(* 4. Zdefiniuj funkcję append przy użyciu fold_right. *)

let append l1 l2 = List.fold_right (fun x y -> x::y) l1 l2;;

append [1;2;3] [5;6];;



