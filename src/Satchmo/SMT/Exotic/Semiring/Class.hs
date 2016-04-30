{-# language TypeSynonymInstances, FlexibleInstances, UndecidableInstances #-}

module Satchmo.SMT.Exotic.Semiring.Class where

data Strictness = Full | Half deriving ( Eq, Ord, Show )

class Semiring a where
    strictness :: a -> Strictness
    nonnegative :: a -> Bool
    strictly_positive :: a -> Bool
    ge :: a -> a -> Bool
    gt :: a -> a -> Bool
    plus  :: a -> a -> a
    zero  :: a
    times :: a -> a -> a
    one   :: a


