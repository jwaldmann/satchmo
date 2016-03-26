module Satchmo.Binary.Op.Common

( iszero
, equals, lt, le, ge, eq, gt
, full_adder, half_adder
, select
, max, min, maximum
)

where

import Prelude hiding ( and, or, not, compare, max, min, maximum )
import qualified Prelude

import qualified Satchmo.Code as C

import Satchmo.Boolean 
   (MonadSAT, Boolean, Booleans
   , fun2, fun3, and, or, not, xor, assertOr, assert, boolean)
import qualified  Satchmo.Boolean as B
import Satchmo.Binary.Data (Number, number, make, bits, width)

import Control.Monad ( forM, foldM )

-- import Satchmo.Counting

import Control.Monad ( forM )

iszero :: (MonadSAT m) =>  Number -> m Boolean
iszero a = equals a $ make []

equals :: (MonadSAT m) =>  Number -> Number -> m Boolean
equals a b = do
    -- equals' ( bits a ) ( bits b )
    let m = Prelude.min ( width a ) ( width b )
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

max :: MonadSAT m => Number -> Number -> m Number
max a b = do
    c <- number $ Prelude.max ( width a ) ( width b )
    ca <- equals c a
    cb <- equals c b
    g <- gt a b
    assert [ not g , ca ]
    assert [     g , cb ]
    return c

min :: MonadSAT m => Number -> Number -> m Number
min a b = do
    c <- number $ Prelude.max ( width a ) ( width b )
    ca <- equals c a
    cb <- equals c b
    g <- lt a b
    assert [ not g , ca ]
    assert [     g , cb ]
    return c

maximum (x:xs) = foldM max x xs

-- | i flag is True, then the number itself, and zero otherwise.
select :: MonadSAT m => Boolean -> Number -> m Number
select flag a = do
    bs <- forM ( bits a ) $ \ b -> and [ flag, b ]
    return $ make bs

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
           -> m ( Boolean , Boolean ) -- ^ (result, carry)
full_adder = full_adder_0

full_adder_1 p1 p2 p3 = do
    p4 <- boolean ; p5 <- boolean
    assert [not p1, not p2, p5]
    assert [not p1, not p3, p5]
    assert [not p1, p4, p5]
    assert [p1, p2, not p5]
    assert [p1, p3, not p5]
    assert [p1, not p4, not p5]
    assert [not p2, not p3, p5]
    assert [not p2, p4, p5]
    assert [p2, p3, not p5]
    assert [p2, not p4, not p5]
    assert [not p3, p4, p5]
    assert [p3, not p4, not p5]
    assert [not p1, not p2, not p3, p4]
    assert [not p1, not p2, p3, not p4]
    assert [not p1, p2, not p3, not p4]
    assert [not p1, p2, p3, p4]
    assert [p1, not p2, not p3, not p4]
    assert [p1, not p2, p3, p4]
    assert [p1, p2, not p3, p4]
    assert [p1, p2, p3, not p4]
    return ( p4, p5 )
       
full_adder_0 p1 p2 p3 = do
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

full_adder_from_half a b c = do
    (p,q) <- half_adder_plain a b
    (r,s) <- half_adder_plain p c
    qs <- or [q,s]
    return ( r, qs )

half_adder :: (MonadSAT m) 
           => Boolean -> Boolean 
           -> m ( Boolean, Boolean ) -- ^ (result, carry)
half_adder = half_adder_plain

half_adder_1 p1 p2 = do
    p3 <- boolean ; p4 <- boolean
    assert [p1, not p4]
    assert [p2, not p4]
    assert [not p3, not p4]
    assert [not p1, not p2, not p3]
    assert [not p1, not p2, p4]
    assert [not p1, p2, p3]
    assert [not p1, p3, p4]
    assert [p1, not p2, p3]
    assert [p1, p2, not p3]
    assert [not p2, p3, p4]
    return (p3,p4)

half_adder_0 p1 p2 = do
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
    -- d <- fun2 ( \ x y -> 1   < s x y ) a b
    d <- and [ a, b ] -- makes three clauses (not four)
    return ( r, d )
