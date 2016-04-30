-- | binary trees without any balance

module Satchmo.SAT.Sequence where

import Data.Monoid
import Data.Foldable

data Seq a = Empty | Leaf !a 
           | Branch !(Seq a) !(Seq a)

{-# INLINABLE singleton #-}
singleton x = Leaf x

instance Monoid (Seq a) where
    {-# INLINABLE mappend #-}
    mappend = Branch
    {-# INLINABLE mempty #-}
    mempty = Empty

instance Foldable Seq where
    {-# INLINABLE fold #-}
    fold s = case s of
        Empty -> mempty
        Leaf k -> k
        Branch l r -> mappend (fold l) (fold r)
        