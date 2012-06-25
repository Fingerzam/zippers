{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, UndecidableInstances, TypeSynonymInstances #-}

class Iterator iter elem | iter -> elem where
    bos      :: iter -> Bool
    eos      :: iter -> Bool
    forward  :: iter -> iter
    backward :: iter -> iter
    value    :: iter -> elem
    update   :: iter -> elem -> iter

class Iterator iter elem => Iterable container iter elem |
  container -> iter, iter -> container where
    open  :: container elem -> iter
    close :: iter -> container elem

instance Iterable [] ([a], a, [a]) a where
    open (x:xs) = ([], x, xs)
    close (prefix, current, suffix) = (reverse prefix) ++ [current] ++ suffix

instance Iterator ([a], a, [a]) a where
    bos ([], _, _) = True
    bos _          = False
    eos (_, _, []) = True
    eos _          = False
    forward (prefix, current, next:suffix)  = (current : prefix, next, suffix)
    backward (prefix, current, next:suffix) = (current : prefix, next, suffix)
    value (_, x, _)                         = x
    update (prefix, _, suffix) new          = (prefix, new, suffix)

data Tree a = Leaf a | Branch (Tree a) (Tree a)
    deriving (Show, Eq)

type TreeIter a = (Tree a, [Either (Tree a) (Tree a)])

up (right, (Left left):p)  = (Branch left right, p)
up (left, (Right right):p) = (Branch left right, p)

descendl c@(Leaf _, p)          = c
descendl (Branch left right, p) = descendl (left, (Right right):p)
descendr c@(Leaf _, p)          = c
descendr (Branch left right, p) = descendl (right, (Left left):p)

isRight (Left _)  = False
isRight (Right _) = True
isLeft            = not . isRight

instance Iterable Tree (TreeIter a) a where
    open tree                     = descendl (tree, [])
    close (tree, [])              = tree
    close (right, (Left left):p)  = close (Branch left right, p)
    close (left, (Right right):p) = close (Branch left right, p)

instance Iterator (Tree a, [Either (Tree a) (Tree a)]) a where
    bos (_, p)                         = all isRight p
    eos (_, p)                         = all isLeft p
    forward    (tree, (Right right):p) = descendl (right, (Left tree):p)
    forward  c@(tree, (Left left):p)   = forward  (up c)
    backward   (tree, (Left left):p)   = descendr (left, (Right tree):p)
    backward c@(tree, (Right right):p) = backward (up c)
    value  (Leaf x, _)                 = x
    update (_, p) x                    = (Leaf x, p)
