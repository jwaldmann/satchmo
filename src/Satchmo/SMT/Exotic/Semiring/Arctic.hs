{-# language DeriveDataTypeable #-}

module Satchmo.SMT.Exotic.Semiring.Arctic where

import Satchmo.SMT.Exotic.Semiring.Class

import Data.Typeable

data Arctic a = Minus_Infinite | Finite a deriving (Eq, Ord, Typeable)

instance Functor Arctic where
    fmap f a = case a of
        Minus_Infinite -> Minus_Infinite
        Finite x -> Finite $ f x

instance Show a => Show ( Arctic a ) where 
    show a = case a of
        Minus_Infinite -> "-"
        Finite x -> show x

instance (Ord a, Num a) => Semiring (Arctic a) where
    strictness _ = Half
    nonnegative a = True -- ??
    strictly_positive a = case a of Finite x -> x >= 0 ; _ -> False
    ge = (>=)
    gt x y = y == Minus_Infinite || (x > y)
    plus = max
    zero = Minus_Infinite
    times x y = case (x,y) of
        (Finite a, Finite b) -> Finite (a+b)
        _ -> Minus_Infinite
    one = Finite 0         
