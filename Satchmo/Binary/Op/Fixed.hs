{-# language MultiParamTypeClasses #-}

-- | operations with fixed bit width.
-- still they are non-overflowing:
-- if overflow occurs, the constraints are not satisfiable.
-- the bit width of the result of binary operations
-- is the max of the bit width of the inputs.

module Satchmo.Binary.Op.Fixed

( restricted
, add, times
, module Satchmo.Binary.Data
, module Satchmo.Binary.Op.Common
)

where

import Prelude hiding ( and, or, not )

import qualified Satchmo.Code as C

import Satchmo.Boolean
import Satchmo.Binary.Data
import Satchmo.Binary.Op.Common
import qualified Satchmo.Binary.Op.Flexible as Flexible

import Satchmo.Counting

-- | give only lower k bits, upper bits must be zero,
-- (else unsatisfiable)
restricted :: Int -> Number -> SAT Number
restricted w a = do
    let ( low, high ) = splitAt w $ bits a
    sequence $ do x <- high ; return $ assert [ not x ]
    return $ make low

-- | result bit width is max of argument bit widths.
-- if overflow occurs, then formula is unsatisfiable.
add :: Number -> Number -> SAT Number
add a b = do
    false <- Satchmo.Boolean.constant False
    let w = max ( width a ) ( width b )
    zs <- add_with_carry w false ( bits a ) ( bits b )
    return $ make zs 

add_with_carry :: Int -> Boolean -> Booleans -> Booleans -> SAT Booleans
add_with_carry w c xxs yys = case ( xxs, yys ) of
    _ | w <= 0 -> do
        sequence_ $ do p <- c : xxs ++ yys ; return $ assert [ not p ]
        return []
    ( [] , [] ) -> return [ c ]
    ( [], y : ys) -> do
        -- r <- xor [ c, y ]
        -- d <- and [ c, y ]
        (r,d) <- half_adder c y
        rest <- add_with_carry (w-1) d [] ys
        return $ r : rest
    ( x : xs, [] ) -> add_with_carry w c yys xxs
    (x : xs, y:ys) -> do
        -- r <- xor [c,x,y]
        -- d <- atleast 2 [c,x,y]
        (r,d) <- full_adder c x y
        rest <- add_with_carry (w-1) d xs ys
        return $ r : rest

-- | result bit width is at most max of argument bit widths.
-- if overflow occurs, then formula is unsatisfiable.
times :: Number -> Number -> SAT Number
times a b = do 
    let w = max ( width a ) ( width b ) 
    restricted_times w a b

restricted_times :: Int -> Number -> Number -> SAT Number
restricted_times w a b = case bits a of
    [] -> return $ make []
    _ | w <= 0 -> do
        monadic assert [ Flexible.iszero a, Flexible.iszero b ]
        return $ make []
    x : xs -> do 
        xys  <- Flexible.times1 x b
        xsys <- if null $ bits b 
                then return $ make [] 
                else do
                       zs <- restricted_times (w-1) b (make xs)
                       Flexible.shift zs
        s <- Flexible.add xys xsys
        restricted w s

        




