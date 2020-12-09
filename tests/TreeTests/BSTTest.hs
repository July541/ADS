{-# LANGUAGE TemplateHaskell #-}

module TreeTests.BSTTest where

import Test.QuickCheck
import Data.List ( sort, elemIndex )
import Data.Maybe

import Tree.BST
import Tree.BinaryTree

import qualified Tree.AVLTree as AVL

prop_bstTest1 :: (Ord a) => [a] -> Property
prop_bstTest1 xs = label "check infix shoule be equal to sorted list" $ toList (fromList xs) == sort xs

prop_bstTest2 :: (Ord a) => [a] -> a -> Property
prop_bstTest2 xs x = label "check exist" $ exist (fromList xs) x == elem x xs

prop_bstTest3 :: (Ord a) => [a] -> a -> Property
prop_bstTest3 xs x = label "refine exist ensure x exist in xs" $ elem x xs ==> exist (fromList xs) x

prop_bstTest4 :: (Ord a) => [a] -> a -> Property
prop_bstTest4 xs x = label "test delete" $ toList (delete (fromList xs) x) == sort deleted
    where
        idx = elemIndex x xs
        deleted = case idx of Nothing -> xs
                              Just n  -> take n xs ++ drop (n + 1) xs

prop_bstTest5 :: (Ord a) => [a] -> a -> Property
prop_bstTest5 xs x = label "check must delete function" $ elem x xs ==> toList (delete (fromList xs) x) == sort deleted
    where
        idx = fromJust $ elemIndex x xs
        deleted = take idx xs ++ drop (idx + 1) xs

prop_bstTest6 :: (Ord a) => [a] -> a -> Property
prop_bstTest6 xs x = label "check insert" $ toList (insert (fromList xs) x) == sort (x:xs)

prop_avlTest1 :: (Ord a) => [a] -> Property
prop_avlTest1 xs = label "" $ toList avl == sort xs && AVL.balanced avl
    where
        avl = AVL.fromList xs

return []

allBstTest = $quickCheckAll