(*
  rlwrap ocaml
*)

let x=1;;
let x=2.;;
let y : float =3.0;;

x=y;;

let f x = x +. 1.;;

f 10.0;;

x;;

f 1. +. f 2.;;

f (1.+.2.);;

f 1. +. 2.;;

let g x y = x+y;;
let g (x:float) (y:float) = x *. y;;

let z = 10;;

g (float_of_int z) 2.;;

float_of_int;;

3.0/.2.;;

-4/5;;

let g (x:float) (y:float) : float = x *. y;;

let rec m n = m n;;

let rec f m n = m n;;

f 1 2;;
f g 2.;;

let z x = x+1;;
z 2;;

f z 2;;

let ff = f g 2.;;
ff 4.;;

(f g 2.) 4.;;

let f = 0;;
let f = fun a -> a+10;;
f 1;;

let rec f m n = m n;;

f (fun a->a+10) "ala";;

g;;
f;;

f g;;
f (f g);;
f (f (f g));;
f f f g;;

let average a b =
  let sum x y = x + y in
  let d = 2 in
  (sum a b) / d;;

sum;;

average 2 3;;

let x=1;;
let r = ref 1;;

r;;
r + 1;;
!r + 1;;
r := 2;;
!r + 1;;

let u = r:=30;;
!r;;

let f x = x;;
f (r:=40);;
!r;;

let rr = ref ();;
rr;;



