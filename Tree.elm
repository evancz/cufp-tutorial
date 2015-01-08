module Tree where
{-| A "Tree" represents a binary tree. A "Node" in a binary tree always has
two children. A tree can also be "Empty". Below I have defined "Tree" and a
number of useful functions.

This example also includes some challenge problems!
-}

import Graphics.Element (..)
import List
import Text (asText)


type Tree a
    = Empty
    | Node a (Tree a) (Tree a)


empty : Tree a
empty =
  Empty


singleton : a -> Tree a
singleton v =
  Node v Empty Empty


insert : comparable -> Tree comparable -> Tree comparable
insert x tree =
  case tree of
    Empty ->
      singleton x

    Node y left right ->
      if  | x == y    -> tree
          | x < y     -> Node y (insert x left) right
          | otherwise -> Node y left (insert x right)


fromList : List comparable -> Tree comparable
fromList xs =
  List.foldl insert empty xs


{-| Determine the depth of a tree. What is the deepest path through the tree?

    depth empty == 0
    depth (singleton 4) == 1
    depth (Node 4 empty (singleton 7)) == 2
-}
depth : Tree a -> Int
depth tree =
  case tree of
    Empty ->
      0

    Node v left right ->
      1 + max (depth left) (depth right)


{-| Apply a function to every value in the tree.

    map sqrt empty == Empty
    map sqrt (singleton 4) == Node 2 Empty Empty
    map sqrt (Node 4 empty (singleton 9)) == Node 2 Empty (Node 3 Empty Empty)
-}
map : (a -> b) -> Tree a -> Tree b
map f tree =
  case tree of
    Empty ->
      Empty

    Node v left right ->
      Node (f v) (map f left) (map f right)


tree1 = fromList [1,2,3]
tree2 = fromList [2,1,3]


main : Element
main =
  asText tree1

{- PRACTICE PROBLEMS

(1) Sum all of the elements of a tree.

      sum : Tree Number -> Number

(2) Flatten a tree into a list.

      flatten : Tree a -> [a]

(3) Check to see if an element is in a given tree.

      isElement : a -> Tree a -> Bool 

(4) Write a general fold function that acts on trees. The fold
    function does not need to guarantee a particular order of
    traversal.

      fold : (a -> b -> b) -> b -> Tree a -> b

(5) Create a reasonable visualization of trees.

      display : Tree a -> Element

    It is important to look into the following functions and libraries to make
    this happen:

      Graphics.Collage
      String.show
      Graphics.Element.asText

    The best visualizations will probably be primarily done with
    Graphics.Collage.

EXTRA EXERCISES:

(a) Use "fold" to do exercises 1-3 in one line each. The best
    readable versions I have come up have the following length
    in characters including spaces and function name:
      sum: 17
      flatten: 23
      isElement: 47
    See if you can match or beat me! Don't forget about currying
    and partial application!

(b) Can "fold" be used to implement "map" or "depth"?

(c) Try experimenting with different ways to traverse a
    tree: pre-order, in-order, post-order, depth-first, etc.
    More info at: http://en.wikipedia.org/wiki/Tree_traversal


-----------------------------------------------------------------}

