module Tools.BasicTools where

class Iterable a where
    {-# MINIMAL next #-}
    next :: a -> a

class BiIterabe a where
    {-# MINIMAL bi_next, bi_prev #-}
    bi_next :: a -> a
    bi_prev :: a -> a