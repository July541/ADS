module Tree.BinaryTree where

class BinaryTree t where
    {-# MINIMAL left, right, value, hasLeft, hasRight,
                isLeaf #-}

    toList :: t a -> [a]
    toList = bInfix

    left :: t a -> t a
    right :: t a -> t a
    value :: t a -> a

    bPrefix :: t a -> [a]
    bPrefix tree
        | isLeaf tree = []
        | otherwise = value tree : bPrefix (left tree) ++ bPrefix (right tree)

    bInfix :: t a -> [a]
    bInfix tree
        | isLeaf tree = []
        | otherwise = bInfix (left tree) ++ [value tree] ++ bInfix (right tree)

    bPostfix :: t a -> [a]
    bPostfix tree
        | isLeaf tree = []
        | otherwise = bPostfix (left tree) ++ bPostfix (right tree) ++ [value tree]

    hasLeft :: t a -> Bool
    hasRight :: t a -> Bool
    isLeaf :: t a -> Bool

    height :: t a -> Int
    height tree
        | isLeaf tree = 0
        | otherwise = max (height $ left tree) (height $ right tree)
