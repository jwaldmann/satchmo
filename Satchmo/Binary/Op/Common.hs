module Satchmo.Binary.Op.Common

( iszero
, equals, lt, le, ge, eq, gt
, full_adder, half_adder
)

where

import Prelude hiding ( and, or, not, compare )

import qualified Satchmo.Code as C

import Satchmo.Boolean hiding ( constant )
import qualified  Satchmo.Boolean as B
import Satchmo.Binary.Data

import Satchmo.Counting

iszero :: Number -> SAT Boolean
iszero a = equals a $ make []

equals :: Number -> Number -> SAT Boolean
equals a b = do
    equals' ( bits a ) ( bits b )


equals' :: Booleans -> Booleans -> SAT Boolean
equals' [] [] = B.constant True
equals' (x:xs) (y:ys) = do
    z <- xor [x, y]
    rest <- equals' xs ys
    and [ not z, rest ]
equals' xs [] = and $ map not xs
equals' [] ys = and $ map not ys

le x y = do (l,e) <- compare x y ; or [l,e]
lt x y = do (l,e) <- compare x y ; return l
ge x y = le y x
gt x y = lt y x
eq = equals

compare :: Number -> Number 
        -> SAT ( Boolean, Boolean )
compare a b = compare' ( bits a ) ( bits b )

compare' :: Booleans 
         -> Booleans 
         -> SAT ( Boolean, Boolean ) -- ^ (less, equals)
compare' [] [] = do 
    f <- B.constant False ; t <- B.constant True ; return ( f, t )
compare' (x:xs) (y:ys) = do
    l <- and [ not x, y ]
    e <- fmap not $ xor [ x, y ]
    ( ll, ee ) <- compare' xs ys
    lee <- and [l,ee]
    l' <- or [ l, lee ] ; e' <- and [ e, ee ]
    return ( l', e' )
compare' xs [] = do
    x <- or xs
    return ( not x, not x )
compare' [] ys = do
    y <- or ys
    return ( y, not y )

full_adder :: Boolean -> Boolean -> Boolean
           -> SAT ( Boolean, Boolean )
full_adder a b c = do
    let s x y z = sum $ map fromEnum [x,y,z]
    r <- fun3 ( \ x y z -> odd $ s x y z ) a b c
    d <- fun3 ( \ x y z -> 1   < s x y z ) a b c
    return ( r, d )

half_adder :: Boolean -> Boolean 
           -> SAT ( Boolean, Boolean )
half_adder a b = do
    let s x y = sum $ map fromEnum [x,y]
    r <- fun2 ( \ x y -> odd $ s x y ) a b
    d <- fun2 ( \ x y -> 1   < s x y ) a b
    return ( r, d )
