module Satchmo.Boolean.Op

( constant
, and, or, xor, equals2, equals, implies
, fun2, fun3
, ifThenElse, ifThenElseM
, assert_fun2, assert_fun3
, monadic
)

where

import Prelude hiding ( and, or, not )
import qualified Prelude
import Control.Applicative ((<$>))
import Satchmo.MonadSAT
import Satchmo.Code
import Satchmo.Boolean.Data

import Satchmo.SAT ( SAT) -- for specializations

import Control.Monad ( foldM, when )

and :: MonadSAT m => [ Boolean ] -> m Boolean
{-# specialize inline and :: [ Boolean ] -> SAT Boolean #-}
and [] = constant True
and [x]= return x
and xs = do
    y <- boolean
    sequence_ $ do
        x <- xs
        return $ assertOr [ not y, x ]
    assertOr $ y : map not xs
    return y

or :: MonadSAT m => [ Boolean ] -> m Boolean
{-# specialize inline or :: [ Boolean ] -> SAT Boolean #-}
or [] = constant False
or [x]= return x
or xs = do
    y <- and $ map not xs
    return $ not y

xor :: MonadSAT m => [ Boolean ] -> m Boolean
{-# specialize inline xor :: [ Boolean ] -> SAT Boolean #-}
xor [] = constant False
xor (x:xs) = foldM xor2 x xs

equals :: MonadSAT m => [ Boolean ] -> m Boolean
equals [] = constant True
equals [x] = constant True
equals (x:xs) = foldM equals2 x xs

equals2 :: MonadSAT m => Boolean -> Boolean -> m Boolean
equals2 a b = not <$> xor2 a b

implies :: MonadSAT m => Boolean -> Boolean -> m Boolean
implies a b = or [not a, b]

ifThenElse :: MonadSAT m => Boolean -> m Boolean -> m Boolean -> m Boolean
ifThenElse condition ifTrue ifFalse = do
  trueBranch <- ifTrue
  falseBranch <- ifFalse
  monadic and [ condition `implies` trueBranch
              , not condition `implies` falseBranch ]

ifThenElseM :: MonadSAT m => m Boolean -> m Boolean -> m Boolean -> m Boolean
ifThenElseM conditionM ifTrue ifFalse = do
  c <- conditionM
  ifThenElse c ifTrue ifFalse

-- | implement the function by giving a full CNF
-- that determines the outcome
fun2 :: MonadSAT m => 
        ( Bool -> Bool -> Bool )
     -> Boolean -> Boolean 
     -> m Boolean
{-# specialize inline fun2 :: (Bool -> Bool -> Bool) -> Boolean -> Boolean -> SAT Boolean #-}
fun2 f x y = do
    r <- boolean
    sequence_ $ do
        a <- [ False, True ]
        b <- [ False, True ]
        let pack flag var = if flag then not var else var
        return $ assertOr
            [ pack a x, pack b y, pack (Prelude.not $ f a b) r ]
    return r

assert_fun2 :: MonadSAT m => 
        ( Bool -> Bool -> Bool )
     -> Boolean -> Boolean 
     -> m ()
{-# specialize inline assert_fun2 :: (Bool -> Bool -> Bool) -> Boolean -> Boolean -> SAT () #-}
assert_fun2 f x y = sequence_ $ do
        a <- [ False, True ]
        b <- [ False, True ]
        let pack flag var = if flag then not var else var
        return $ when ( Prelude.not $ f a b ) $ assert 
            [ pack a x, pack b y ]
     

-- | implement the function by giving a full CNF
-- that determines the outcome
fun3 :: MonadSAT m => 
        ( Bool -> Bool -> Bool -> Bool )
     -> Boolean -> Boolean -> Boolean
     -> m Boolean
{-# specialize inline fun3 :: (Bool -> Bool -> Bool -> Bool) -> Boolean -> Boolean -> Boolean -> SAT Boolean #-}
fun3 f x y z = do
    r <- boolean
    sequence_ $ do
        a <- [ False, True ]
        b <- [ False, True ]
        c <- [ False, True ]
        let pack flag var = if flag then not var else var
        return $ assertOr
            [ pack a x, pack b y, pack c z
            , pack (Prelude.not $ f a b c) r 
            ]
    return r

assert_fun3 :: MonadSAT m => 
        ( Bool -> Bool -> Bool -> Bool )
     -> Boolean -> Boolean -> Boolean
     -> m ()
{-# specialize inline assert_fun3 :: (Bool -> Bool -> Bool -> Bool) -> Boolean -> Boolean -> Boolean -> SAT () #-}
assert_fun3 f x y z = sequence_ $ do
        a <- [ False, True ]
        b <- [ False, True ]
        c <- [ False, True ]
        let pack flag var = if flag then not var else var
        return $ when ( Prelude.not $ f a b c ) $ assert 
            [ pack a x, pack b y, pack c z ]
     

xor2 :: MonadSAT m => Boolean -> Boolean -> m Boolean
{-# specialize inline  xor2 :: Boolean -> Boolean -> SAT Boolean #-}
xor2 = fun2 (/=)
-- xor2 = xor2_orig

-- for historic reasons:
xor2_orig :: MonadSAT m => Boolean -> Boolean -> m Boolean
xor2_orig x y = do
    a <- and [ x, not y ]
    b <- and [ not x, y ]
    or [ a, b ]

