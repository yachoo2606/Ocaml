let f a l =
  let multiply x = x * a in
    List.map multiply l;;
    
f 2 [1;2;3];;    
    
let double = f 2;;
let triple = f 3;;
double [1;2;3];;
triple [1;2;3];;

let k=1::2::3::[];;
let k=[5;6;7];;

let w = [1;2]=k;;
w=w;;
[5;6;7]=k;;

4 > 3;;
4 < 3;;
4 <> 3;;
4 >= 3;;
4 <= 3;;

match [1;2]=k with 
  true -> 1
| false -> 0;;

match w with 
  true -> 1
| false -> 0;;

if w then 1 else 0;;

match k with 
  [a;2] -> a
| [a;b] -> a+b
| a::b::c::[] -> a+b
| _ -> -1;;

let rec len l =
  match l with 
    [] -> 0
  | head::rest -> 1 + len rest;;
  
len k;;
k;;

len [["0"];["ala"; "kot"];[]];;

9.3 > 5.1;;
9.3 +. 5.1;;

(* variant 1 - wrong *)

let rec range x y = 
  if x > y then 
    []
  else 
    x :: range (x+1) y;;

range 10 1;;

range 5 10;;
range 1 1000;;
range (-3) 10;;

range 1 1000000000000;; (* Stack overflow *)

(* variant 2 - ok, but inefficient *)

let range a b = 
  let rec f x y accum = 
    if x > y then 
      accum 
    else
       f (x+1) y (accum@[x])
  in
     f a b [];;

range 1 10;;

(* variant 3 *)

let range a b = 
  let rec f x y accum = 
     if x > y then 
       accum 
     else 
       f x (y-1) (y::accum)
  in
    f a b [];;

(* variant 4 *)

let range a b = 
  let rec f y accum = 
     if a > y then 
       accum 
     else 
       f (y-1) (y::accum)
  in
    f b [];; 
    
range 1 10;;
range 1 1000000000000;;

(1,2);;
let triple = (1,"ala",'x');;

[1]@[2;3];;

"ala" ^ " i kot" ^ ".";;

let f m =
  match m with 
    (x,y,_) when x=y -> 0
  | (x,y,z) -> x+y+z;;
 
f (1,1,3);;

let f = function
    (x,y,_) when x=y -> 0
  | (x,y,z) -> x+y+z;;

f (1,2,3);;
