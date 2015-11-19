data Tree a = Empty | Node (Tree a) a (Tree a)
            deriving (Show,Eq)

data TreeDir = L | R

valAt :: Tree a -> [TreeDir] -> a
valAt (Node _ v _) [] = v
valAt (Node l _ r) (L:ds) = valAt l ds
valAt (Node l _ r) (R:ds) = valAt r ds


tree1 :: Tree Char
tree1 = Node (Node (Node Empty 'A' Empty)
                   'B'
                   (Node Empty 'C' Empty))
             'D'
             (Node (Node Empty 'E' Empty)
                   'F'
                   Empty)

ex1 = valAt tree1 [] -- root, i.e. D
ex2 = valAt tree1 [L,L] -- A
ex3 = valAt tree1 [R,L] -- E

-- -- -- -- -- -- -- -- -- -- --

-- D(1 + a*x*x) = 2*a*x = a*x + a*x
data TreePath a = GoLeft {- HOLE -} a (Tree a) | GoRight (Tree a) a {- HOLE -}
                deriving (Show,Eq)

data TreeZipper a = TreeZipper [TreePath a] (Tree a)
                  deriving (Show,Eq)

start :: Tree a -> TreeZipper a
start t = TreeZipper [] t

goLeft :: TreeZipper a -> TreeZipper a
goLeft (TreeZipper path (Node l v r)) = TreeZipper (GoLeft v r : path) l

goRight :: TreeZipper a -> TreeZipper a
goRight (TreeZipper path (Node l v r)) = TreeZipper (GoRight l v : path) r

goUp :: TreeZipper a -> TreeZipper a
goUp (TreeZipper (GoLeft v r : path) l) = TreeZipper path (Node l v r)
goUp (TreeZipper (GoRight l v : path) r) = TreeZipper path (Node l v r)

currentVal :: TreeZipper a -> a
currentVal (TreeZipper _ (Node _ v _)) = v

ex4 = goLeft $ goLeft $ start tree1
-- => TreeZipper [GoLeft 'B' (Node Empty 'C' Empty),
--                GoLeft 'D' (Node (Node Empty 'E' Empty) 'F' Empty)]
--               (Node Empty 'A' Empty)

-- -- -- -- -- -- -- -- -- -- --
