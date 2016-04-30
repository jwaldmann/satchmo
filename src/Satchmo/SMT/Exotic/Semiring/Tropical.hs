{-# language DeriveDataTypeable #-}

module Satchmo.SMT.Exotic.Semiring.Tropical where

import Satchmo.SMT.Exotic.Semiring.Class

import Data.Typeable

data Tropical a = Finite a | Plus_Infinite deriving (Eq, Ord, Typeable)

instance Functor Tropical where
    fmap f a = case a of
        Finite x -> Finite $ f x
        Plus_Infinite -> Plus_Infinite

instance Show a => Show ( Tropical a ) where 
    show a = case a of
        Plus_Infinite -> "+"
        Finite x -> show x

instance (Ord a, Num a) => Semiring (Tropical a) where
    strictness _ = Half
    nonnegative a = True -- ??
    strictly_positive a = case a of Finite x -> x >= 0 ; _ -> False
    ge = (>=)
    gt x y = x == Plus_Infinite || (x > y)
    plus = min
    zero = Plus_Infinite
    times x y = case (x,y) of
        (Finite a, Finite b) -> Finite (a+b)
        _ -> Plus_Infinite
    one = Finite 0         
