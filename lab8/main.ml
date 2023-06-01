type 'a btree = 
| Leaf of 'a
| Empty
| Node of 'a btree * 'a btree

type btree = int btree

module TMap = Map.Make(
  struct
   type t = btree
   type t = int
   let compare = compare
  end);;