module Tree.AVLTree where

import Tree.BinaryTree
import Tree.BST

newtype AVLTree a = AVLTree (BST a)

instance Functor AVLTree where
    fmap f (AVLTree t) =  AVLTree $ fmap f t

instance BinaryTree AVLTree where
    left (AVLTree t) = AVLTree $ left t
    right (AVLTree t) = AVLTree $ right t
    value (AVLTree t) = value t

    is_leaf (AVLTree t) = is_leaf t

    has_left (AVLTree t) = has_left t
    has_right (AVLTree t) = has_right t

    fromList = AVLTree . fromList

    b_prefix (AVLTree t) = b_prefix t
    b_infix (AVLTree t) = b_infix t
    b_postfix (AVLTree t) = b_postfix t

    height (AVLTree Leaf) = -1
    height (AVLTree (Node l _ r)) = 1 + (height (AVLTree l) `max` height (AVLTree r))