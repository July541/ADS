module Tree.AVLTree where

import Tree.BinaryTree
import Tree.BST

newtype AVLTree a = AVLTree (BST a) deriving (Show, Eq)

instance Functor AVLTree where
    fmap f (AVLTree t) =  AVLTree $ fmap f t

instance BinaryTree AVLTree where
    left (AVLTree (BSTNode l _ _)) = AVLTree l
    right (AVLTree (BSTNode _ _ r)) = AVLTree r
    value (AVLTree (BSTNode _ v _)) = v

    hasLeft (AVLTree bst) = hasLeft bst
    hasRight (AVLTree bst) = hasLeft bst
    isLeaf (AVLTree bst) = isLeaf bst

on :: (BST a -> BST a) -> AVLTree a -> AVLTree a
on f (AVLTree bst) = AVLTree (f bst)

avlHeight :: (BinaryTree t) => t a -> Int
avlHeight tree
    | isLeaf tree = -1
    | otherwise = 1 + avlHeight (left tree) `max` avlHeight (right tree)

balanceFactor :: (BinaryTree t) => t a -> Int
balanceFactor tree
    | isLeaf tree = 0
    | otherwise = avlHeight (left tree) - avlHeight (right tree)

balanced :: BinaryTree t => t a -> Bool
balanced tree
    | isLeaf tree = True
    | not $ balanced $ left tree = False
    | not $ balanced $ right tree = False
    | otherwise = abs (avlHeight (left tree) - avlHeight (right tree)) < 2

fromList :: (Ord a) => [a] -> AVLTree a
fromList = foldl Tree.AVLTree.insert (AVLTree BSTLeaf)

insert :: (Ord a) => AVLTree a -> a -> AVLTree a
insert avl x = apply rotate $ flip Tree.BST.insert x `on` avl
    where
        apply f (AVLTree t) = AVLTree (f t)

empty :: AVLTree a
empty = AVLTree BSTLeaf

rotate :: (Ord a) => BST a -> BST a
rotate t@(BSTNode l v r)
    | not (balanced l) = BSTNode (rotate l) v r
    | not (balanced r) = BSTNode l v (rotate r)
    | balanceFactor t == -2 && balanceFactor r == -1 -- RR
        = BSTNode (BSTNode l v (left r)) (value r) (right r)
    | balanceFactor t == 2 && balanceFactor l == 1 -- LL
        = BSTNode (left l) (value l) (BSTNode (right l) v r)
    | balanceFactor t == 2 && balanceFactor l == -1 -- LR
        = BSTNode (BSTNode (left l) (value l) (left $ right l))
                  (value $ right l)
                  (BSTNode (right $ right l) v r)
    | balanceFactor t == -2 && balanceFactor r == 1 -- RL
        = BSTNode (BSTNode l v (left $ left r))
                  (value $ left r)
                  (BSTNode (right $ left r) (value r) (right r))
    | otherwise = t
