module Satchmo.Binary.Op.Common

( iszero
, equals, lt, le, ge, eq, gt
, full_adder, half_adder
)

where

import Prelude hiding ( and, or, not, compare )

import qualified Satchmo.Code as C

import Satchmo.Boolean (MonadSAT, Boolean, Booleans, fun2, fun3, and, or, not, xor)
import qualified  Satchmo.Boolean as B
import Satchmo.Binary.Data (Number, make, bits)

import Satchmo.Counting

iszero :: (MonadSAT m) =>  Number -> m Boolean
iszero a = equals a $ make []

equals :: (MonadSAT m) =>  Number -> Number -> m Boolean
equals a b = do
    equals' ( bits a ) ( bits b )


equals' :: (MonadSAT m) =>  Booleans -> Booleans -> m Boolean
equals' [] [] = B.constant True
equals' (x:xs) (y:ys) = do
    z <- xor [x, y]
    rest <- equals' xs ys
    and [ not z, rest ]
equals' xs [] = and $ map not xs
equals' [] ys = and $ map not ys

le,lt,ge,gt,eq :: MonadSAT m => Number -> Number -> m Boolean
le x y = do (l,e) <- compare x y ; or [l,e]
lt x y = do (l,e) <- compare x y ; return l
ge x y = le y x
gt x y = lt y x
eq = equals

compare :: MonadSAT m => Number -> Number 
        -> m ( Boolean, Boolean )
compare a b = compare' ( bits a ) ( bits b )

compare' :: (MonadSAT m) => Booleans 
         -> Booleans 
         -> m ( Boolean, Boolean ) -- ^ (less, equals)

compare' [] [] = do 
    f <- B.constant False ; t <- B.constant True ; return ( f, t )
compare' (x:xs) (y:ys) = do
    l <- and [ not x, y ]
    e <- fmap not $ xor [ x, y ]
    ( ll, ee ) <- compare' xs ys
    lee <- and [l,ee]
    l' <- or [ ll, lee ]
    e' <- and [ e, ee ]
    return ( l', e' )
compare' xs [] = do
    x <- or xs
    return ( x, not x )
compare' [] ys = do
    y <- or ys
    return ( y, not y )

full_adder :: (MonadSAT m) => Boolean -> Boolean -> Boolean
           -> m ( Boolean, Boolean )
full_adder a b c = do
    let s x y z = sum $ map fromEnum [x,y,z]
    r <- fun3 ( \ x y z -> odd $ s x y z ) a b c
    d <- fun3 ( \ x y z -> 1   < s x y z ) a b c
    return ( r, d )

half_adder :: (MonadSAT m) => Boolean -> Boolean 
           -> m ( Boolean, Boolean )
half_adder a b = do
    let s x y = sum $ map fromEnum [x,y]
    r <- fun2 ( \ x y -> odd $ s x y ) a b
    d <- fun2 ( \ x y -> 1   < s x y ) a b
    return ( r, d )
