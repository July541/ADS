module Tree.BinaryTree where

class BinaryTree t where
    {-# MINIMAL left, right, value, has_left, has_right,
                is_leaf, fromList #-}

    fromList :: (Ord a) => [a] -> t a

    toList :: t a -> [a]
    toList = b_infix

    left :: t a -> t a
    right :: t a -> t a
    value :: t a -> a

    b_prefix :: t a -> [a]
    b_prefix tree
        | is_leaf tree = []
        | otherwise = value tree : b_prefix (left tree) ++ b_prefix (right tree)

    b_infix :: t a -> [a]
    b_infix tree
        | is_leaf tree = []
        | otherwise = b_infix (left tree) ++ [value tree] ++ b_infix (right tree)

    b_postfix :: t a -> [a]
    b_postfix tree
        | is_leaf tree = []
        | otherwise = b_postfix (left tree) ++ b_postfix (right tree) ++ [value tree]

    has_left :: t a -> Bool
    has_right :: t a -> Bool
    is_leaf :: t a -> Bool

    height :: t a -> Int
    height tree
        | is_leaf tree = 0
        | otherwise = max (height $ left tree) (height $ right tree)
