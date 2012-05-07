module Satchmo.SMT.Exotic.Semiring.Fuzzy where

import Satchmo.SMT.Exotic.Semiring.Class

data Fuzzy a = Minus_Infinite | Finite a | Plus_Infinite deriving (Eq, Ord)

instance Functor Fuzzy where
    fmap f a = case a of
        Minus_Infinite -> Minus_Infinite
        Finite x -> Finite $ f x
        Plus_Infinite -> Plus_Infinite

instance Show a => Show ( Fuzzy a ) where 
    show a = case a of
        Minus_Infinite -> "-"
        Finite x -> show x
        Plus_Infinite -> "+"

instance (Ord a, Num a) => Semiring (Fuzzy a) where
    strictness _ = Half
    nonnegative a = case a of Plus_Infinite -> False ; _ -> True
    strictly_positive = nonnegative -- CHECK
    ge x y = x == Plus_Infinite || (x > y) || y == Minus_Infinite 
    gt x y = x == Plus_Infinite || (x > y)
    plus = min
    zero = Plus_Infinite
    times = max
    one = Minus_Infinite
