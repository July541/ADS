module Tree.BST where

import Tree.BinaryTree

data BST a = BSTLeaf | BSTNode (BST a) a (BST a) deriving (Show, Eq)

instance Functor BST where
    fmap _ BSTLeaf = BSTLeaf
    fmap f (BSTNode l v r) = BSTNode (fmap f l) (f v) (fmap f r)

instance BinaryTree BST where
    left (BSTNode l _ _) = l
    right (BSTNode _ _ r) = r
    value (BSTNode _ v _) = v

    is_leaf BSTLeaf = True
    is_leaf _    = False

    has_left BSTLeaf = False
    has_left (BSTNode BSTLeaf _ _) = False
    has_left _ = True

    has_right BSTLeaf = False
    has_right (BSTNode _ _ BSTLeaf) = False
    has_right _ = True

    fromList [] = BSTLeaf
    fromList (x:xs) = let l = filter (<x) xs
                          r = filter (>=x) xs
                      in  BSTNode (fromList l) x (fromList r)
