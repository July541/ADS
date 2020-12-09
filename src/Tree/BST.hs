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

    isLeaf BSTLeaf = True
    isLeaf _    = False

    hasLeft BSTLeaf = False
    hasLeft (BSTNode BSTLeaf _ _) = False
    hasLeft _ = True

    hasRight BSTLeaf = False
    hasRight (BSTNode _ _ BSTLeaf) = False
    hasRight _ = True

fromList :: Ord a => [a] -> BST a
fromList = foldl insert BSTLeaf

exist :: (Ord a) => BST a -> a -> Bool
exist BSTLeaf _ = False
exist (BSTNode l v r) x
    | x == v = True
    | x < v  = exist l x
    | x > v  = exist r x

insert :: (Ord a) => BST a -> a -> BST a
insert BSTLeaf x = BSTNode BSTLeaf x BSTLeaf
insert (BSTNode l v r) x = if x < v then BSTNode (insert l x) v r
                                    else BSTNode l v (insert r x)

delete :: (Ord a) => BST a -> a -> BST a
delete BSTLeaf _ = BSTLeaf
delete (BSTNode l v r) x
    | x < v = BSTNode (delete l x) v r
    | x > v = BSTNode l v (delete r x)
    | x == v = if isLeaf r then l
               else if isLeaf l then r
               else BSTNode (deleteRightMost l) (rightMost l) r
    where
        rightMost :: BST a -> a
        rightMost (BSTNode _ v r) = if isLeaf r then v else rightMost r

        deleteRightMost :: BST a -> BST a
        deleteRightMost (BSTNode l v r) = if isLeaf r then l else BSTNode l v (deleteRightMost r)