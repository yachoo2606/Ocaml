(*
  rlwp ocaml   
*)

let k = 1::2::[3];;

let w = k = [1;2];;

if w then 1 else 0;;

if w then string_of_int 1 else "0";;

match k with 
    [a;2] -> 0
    | [a;b;c] -> a+b+c
    | _ -> -1;;

match [2;1] with 
    [a;b] when a=b -> 0
    | [a;b;c] -> a+b+c
    | _ -> -1;;
 


let rec len l =
    match l with
        [] -> 0
        | a::tail -> 1 + len tail;;

let lista = [1;2;3;4;5;67;54;2;1;345;6543;2;2];;
len lista;;

(* zle bo nie tail recursion ....  *)

let rec range x y = 
    if x > y then 
        []
    else
        x:: range (x+1) y;;

range 5 1000000;;


(* dobrze teraz bo juz tail recurtion - i tak nie do konca dobre............*)

let rangeBetter a b =
    let rec helper x y accum = 
        if x > y then
            accum
        else
            helper (x+1) y (accum@[x])
    in
        helper a b []
;;


rangeBetter 2 5;;

(* jeszcze lepszy program.... niby *)

let rangeEvenBetter a b =
    let rec helper x y accum = 
        if x > y then
            accum
        else
            helper x (y-1) (y::accum)
    in
        helper a b []
;;

rangeEvenBetter 1 100000000;;

(* zadna roznica tylko wywalenie zmiennej.... *)

let rangeEvenBetterBetter a b =
    let rec helper y accum = 
        if a > y then
            accum
        else
            helper (y-1) (y::accum)
    in
        helper b []
;;

rangeEvenBetterBetter 1 10000000000;;