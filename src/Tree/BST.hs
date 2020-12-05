module Tree.BST where

import Tree.BinaryTree

data BST a = Leaf | Node (BST a) a (BST a) deriving (Eq)

instance BinaryTree BST where
    left (Node l _ _) = l
    right (Node _ _ r) = r
    value (Node _ v _) = v

    is_leaf Leaf = True
    is_leaf _    = False

    has_left Leaf = False
    has_left (Node Leaf _ _) = False
    has_left _ = True

    has_right Leaf = False
    has_right (Node _ _ Leaf) = False
    has_right _ = True

    fromList [] = Leaf
    fromList (x:xs) = let l = filter (>x) xs
                          r = filter (<=x) xs
                      in  Node (fromList l) x (fromList r)

    b_prefix Leaf = []
    b_prefix (Node l v r) = v : b_prefix l ++ b_prefix r

    b_infix Leaf = []
    b_infix (Node l v r) = b_infix l ++ [v] ++ b_infix r

    b_postfix Leaf = []
    b_postfix (Node l v r) = b_infix l ++ b_infix r ++ [v]