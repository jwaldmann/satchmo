module Satchmo.Boolean.Op 

( constant
, and, or, xor
, fun2, fun3
, monadic
)

where

import Prelude hiding ( and, or, not )
import qualified Prelude

import Satchmo.Internal
import Satchmo.Code
import Satchmo.Boolean.Data

import Control.Monad ( foldM )

and :: [ Boolean ] -> SAT Boolean
and xs = do
    y <- boolean
    sequence $ do
        x <- xs
        return $ assert [ not y, x ]
    assert $ y : map not xs
    return y

or :: [ Boolean ] -> SAT Boolean
or xs = do
    y <- and $ map not xs
    return $ not y

xor :: [ Boolean ] -> SAT Boolean
xor [] = constant False
xor (x:xs) = foldM xor2 x xs


-- | implement the function by giving a full CNF
-- that determines the outcome
fun2 :: ( Bool -> Bool -> Bool )
     -> Boolean -> Boolean 
     -> SAT Boolean
fun2 f x y = do
    r <- boolean
    sequence_ $ do
        a <- [ False, True ]
        b <- [ False, True ]
        let pack flag var = if flag then not var else var
        return $ assert 
            [ pack a x, pack b y, pack (Prelude.not $ f a b) r ]
    return r

-- | implement the function by giving a full CNF
-- that determines the outcome
fun3 :: ( Bool -> Bool -> Bool -> Bool )
     -> Boolean -> Boolean -> Boolean
     -> SAT Boolean
fun3 f x y z = do
    r <- boolean
    sequence_ $ do
        a <- [ False, True ]
        b <- [ False, True ]
        c <- [ False, True ]
        let pack flag var = if flag then not var else var
        return $ assert 
            [ pack a x, pack b y, pack c z
            , pack (Prelude.not $ f a b c) r 
            ]
    return r

xor2 :: Boolean -> Boolean -> SAT Boolean
xor2 = fun2 (/=)

-- for historic reasons:
xor2_orig :: Boolean -> Boolean -> SAT Boolean
xor2_orig x y = do
    a <- and [ x, not y ]
    b <- and [ not x, y ]
    or [ a, b ]


