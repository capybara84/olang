type integer = int
type c = char and s = string
type x = char and y = string and z = int
type f = unit -> int
type f = int -> int -> int
type 'a f = 'a -> 'a -> 'a
type 'a x = 'a
type t = int * char
type f = (float)
type l = int list
type 'a r = 'a ref
type 'a pair = ('a * 'a)
type ('a, 'b) pair = ('a * 'b)
type point2d = { mutable x :: int; mutable y :: int }
type 'a point2d = { x :: 'a; y :: 'a }
type ('a,'b,'c) atob = { a :: 'a; b :: 'b; c :: 'c }
type color = | Red | Green | Blue
type color = Red | Green | Blue
type color = Red | Green | Blue | RGB (int * int * int)
type 'a option = None | Some 'a
type 'a tree = Node 'a | Leaf ('a tree * 'a tree)
type itree = int tree
type lt = List.t

