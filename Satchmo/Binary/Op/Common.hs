module Satchmo.Binary.Op.Common

( iszero
, equals, lt, le, ge, eq, gt
, full_adder, half_adder
)

where

import Prelude hiding ( and, or, not, compare )

import qualified Satchmo.Code as C

import Satchmo.Boolean 
   (MonadSAT, Boolean, Booleans
   , fun2, fun3, and, or, not, xor, assertOr, boolean)
import qualified  Satchmo.Boolean as B
import Satchmo.Binary.Data (Number, make, bits, width)

import Control.Monad ( forM )

import Satchmo.Counting

iszero :: (MonadSAT m) =>  Number -> m Boolean
iszero a = equals a $ make []

equals :: (MonadSAT m) =>  Number -> Number -> m Boolean
equals a b = do
    -- equals' ( bits a ) ( bits b )
    let m = min ( width a ) ( width b )
    let ( a1, a2 ) = splitAt m $ bits a
    let ( b1, b2 ) = splitAt m $ bits b
    common <- forM ( zip a1 b1 ) $ \ (x,y) -> fun2 (==) x y
    and $ common ++ map not ( a2 ++ b2 ) 
    
equals' :: (MonadSAT m) =>  Booleans -> Booleans -> m Boolean
equals' [] [] = B.constant True
equals' (x:xs) (y:ys) = do
    z <- fun2 (==) x y
    rest <- equals' xs ys
    and [ z, rest ]
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
    f <- B.constant False 
    t <- B.constant True 
    return ( f, t )
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
    never <- B.constant False
    return ( never, not x )
compare' [] ys = do
    y <- or ys
    return ( y, not y )

full_adder :: (MonadSAT m) 
           => Boolean -> Boolean -> Boolean
           -> m ( Boolean, Boolean )
full_adder p1 p2 p3 = do
    p4 <- boolean ; p5 <- boolean
    assertOr [not p2,p4,p5]
    assertOr [p2,not p4,not p5]
    assertOr [not p1,not p3,p5]
    assertOr [not p1,not p2,not p3,p4]
    assertOr [not p1,not p2,p3,not p4]
    assertOr [not p1,p2,p3,p4]
    assertOr [p1,p3,not p5]
    assertOr [p1,not p2,not p3,not p4]
    assertOr [p1,p2,not p3,p4]
    assertOr [p1,p2,p3,not p4]
    return ( p4, p5 )

full_adder_plain a b c = do
    let s x y z = sum $ map fromEnum [x,y,z]
    r <- fun3 ( \ x y z -> odd $ s x y z ) a b c
    d <- fun3 ( \ x y z -> 1   < s x y z ) a b c
    return ( r, d )

half_adder :: (MonadSAT m) 
           => Boolean -> Boolean 
           -> m ( Boolean, Boolean )
half_adder p1 p2 = do
    p3 <- boolean ; p4 <- boolean
    assertOr [not p2,p3,p4]
    assertOr [p2,not p4]
    assertOr [not p1,p3,p4]
    assertOr [not p1,not p2,not p3]
    assertOr [p1,not p4]
    assertOr [p1,p2,not p3]
    return ( p3, p4 )

half_adder_plain a b = do
    let s x y = sum $ map fromEnum [x,y]
    r <- fun2 ( \ x y -> odd $ s x y ) a b
    d <- fun2 ( \ x y -> 1   < s x y ) a b
    return ( r, d )
