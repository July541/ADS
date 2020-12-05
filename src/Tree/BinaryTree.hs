module Tree.BinaryTree where

class BinaryTree t where
    {-# MINIMAL left, right, value, has_left, has_right, is_leaf,
                fromList, b_prefix, b_postfix, (b_infix | toList) #-}
    fromList :: (Ord a) => [a] -> t a
    toList :: t a -> [a]
    toList = b_infix
    left :: t a -> t a
    right :: t a -> t a
    value :: t a -> a
    b_prefix :: t a -> [a]
    b_infix :: t a -> [a]
    b_infix = toList
    b_postfix :: t a -> [a]
    has_left :: t a -> Bool
    has_right :: t a -> Bool
    is_leaf :: t a -> Bool
