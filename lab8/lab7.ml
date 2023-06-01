(* moduł Map *)

(* wariant 1 - klucz standardowego typu *)

module MyMap = Map.Make (String);; 

(* wariant 2 - niestandardowe klucze *)

module MyMap = Map.Make (
  struct 
     type t = string (* mapa z kluczem typu string *)
     let compare = compare 
  end);;
    
let m = MyMap.empty;;
    
let m = MyMap.add "a" 1 m;; 
let m = MyMap.add "b" 2 m;;  
let m = MyMap.add "c" 3 m;;  
let m = MyMap.add "d" 4 m;;    
    
MyMap.find "c" m;;    

let wypisz k v =
  print_string (k^" "^string_of_int v ^"\n");;

MyMap.iter wypisz m;;

let m = MyMap.add "e" 5 m;;
MyMap.iter wypisz m;; 

let m2 = MyMap.add "f" 6 m;;
MyMap.iter wypisz m2;; 
MyMap.iter wypisz m;; 

let m = MyMap.remove "b" m;;

let m3 = MyMap.remove "a" m;;
MyMap.iter wypisz m3;;

let m = MyMap.add "a" 100 m;; 
MyMap.iter wypisz m;; 
let m = MyMap.add "a" "Hello!" m;;

try
  Printf.printf "%d\n" (MyMap.find "fgfgfa" m)
with
   Not_found -> print_string "No value found.\n";;
   
MyMap.mem "dwfsefew" m;; 

let nm = MyMap.map (fun v -> v*2) m;;  
   
MyMap.iter wypisz m;; 
MyMap.iter wypisz nm;; 
   
let nm = MyMap.mapi (fun k v -> k^string_of_int v) m;; 
   
let wypisz2 k v =
  print_string (k^" "^v ^"\n");;

MyMap.iter wypisz2 nm;;

let nm = MyMap.fold (fun k v r -> r-v) m 0;;

let b = MyMap.equal (fun a b -> a=b) m m2;;


(* Ćwiczenie:

- należy zdefiniować moduł TMap dla drzew, w których kluczem jest typ 'a btree,
  a wartością int
- dwa klucze drzewiaste są identyczne jeśli mają tę samą strukturę i te same
  elementy w liściach
- należy sprawdzić działanie przykładowych funkcji w module TMap (jak wyżej). *)
