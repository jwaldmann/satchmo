{-# language MultiParamTypeClasses #-}

-- | operations with fixed bit width.
-- still they are non-overflowing:
-- if overflow occurs, the constraints are not satisfiable.
-- the bit width of the result of binary operations
-- is the max of the bit width of the inputs.

module Satchmo.Binary.Op.Fixed

( restricted
, add, times, restrictedTimes, dot_product
, module Satchmo.Binary.Data
, module Satchmo.Binary.Op.Common
)

where

import Prelude hiding ( and, or, not, min, max )
import qualified Prelude
import Control.Monad (foldM)

import qualified Satchmo.Code as C

import Satchmo.Boolean
import Satchmo.Binary.Data
import Satchmo.Binary.Op.Common
import qualified Satchmo.Binary.Op.Flexible as Flexible

import Satchmo.Counting

import Control.Monad ( forM, when )

import Data.Map ( Map )
import qualified Data.Map as M

-- | give only lower k bits, upper bits must be zero,
-- (else unsatisfiable)
restricted :: (MonadSAT m) => Int -> Number -> m Number
restricted w a = do
    let ( low, high ) = splitAt w $ bits a
    sequence $ do x <- high ; return $ assertOr [ not x ]
    return $ make low

-- | result bit width is max of argument bit widths.
-- if overflow occurs, then formula is unsatisfiable.
add :: (MonadSAT m) => Number -> Number -> m Number
add a b = do
    false <- Satchmo.Boolean.constant False
    let w = Prelude.max ( width a ) ( width b )
    zs <- add_with_carry w false ( bits a ) ( bits b )
    return $ make zs 

add_with_carry :: (MonadSAT m) => Int -> Boolean -> Booleans -> Booleans -> m Booleans
add_with_carry w c xxs yys = case ( xxs, yys ) of
    _ | w <= 0 -> do
        sequence_ $ do p <- c : xxs ++ yys ; return $ assertOr [ not p ]
        return []
    ( [] , [] ) -> return [ c ]
    ( [], y : ys) -> do
        (r,d) <- half_adder c y
        rest <- add_with_carry (w-1) d [] ys
        return $ r : rest
    ( x : xs, [] ) -> add_with_carry w c yys xxs
    (x : xs, y:ys) -> do
        (r,d) <- full_adder c x y
        rest <- add_with_carry (w-1) d xs ys
        return $ r : rest

-- | result bit width is at most max of argument bit widths.
-- if overflow occurs, then formula is unsatisfiable.
times :: (MonadSAT m) => Number -> Number -> m Number
times a b = do 
    let w = Prelude.max ( width a ) ( width b ) 
    better_times w a b

-- Ignores overflows
restrictedAdd :: (MonadSAT m) => Number -> Number -> m Number
restrictedAdd a b = do 
  zero <- Satchmo.Boolean.constant False
  (result, _) <- Flexible.add_with_carry zero (bits a) (bits b)
  return $ make result

-- Ignores overflows
restrictedShift :: (MonadSAT m) => Number -> m Number
restrictedShift a = do
  zero <- Satchmo.Boolean.constant False
  return $ make $ zero : (take (width a - 1) $ bits a)

-- Ignores overflows
restrictedTimes :: (MonadSAT m) => Number -> Number -> m Number
restrictedTimes as bs = do
  result <- foldM (\(as',sum) b -> do
                       summand <- Flexible.times1 b as'
                       sum'    <- sum `restrictedAdd` summand
                       nextAs' <- restrictedShift as'
                       return (nextAs', sum')
                  ) (as, make []) $ bits bs
  return $ snd result

{-
case bits a of
    [] -> return $ make []
    _ | w <= 0 -> do
        monadic assertOr [ Flexible.iszero a, Flexible.iszero b ]
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
-}

-- | this is used in matrix multiplication,
-- so we try to provide an optimized implementation here.
-- argument vectors must have equal length
dot_product :: ( MonadSAT m )
            => Int -- ^ result bit width
            -> [ Number ] 
            -> [ Number ] 
            -> m Number
dot_product w xs ys = do
    when ( length xs /= length ys ) 
         $ error "Satchmo.Binary.Op.Fixed.dot_product: vector lengths differ"
    kzss <- forM ( zip xs ys ) $ \ (a,b) -> particles w a b
    combine w $ concat kzss

combine w kzs = do
    zs <- reduce $ take w
                 $ M.elems $ M.fromListWith (++) kzs
    return $ make zs    

-------------------------------------------------- 

better_times w a b = do
    kzs <- particles w a b
    combine w kzs

particles w a b = sequence $ do
          ( i , x ) <- zip [ 0 .. ] $ bits a
          ( j , y ) <- zip [ 0 .. ] $ bits b
          return $ 
              if i+j >= w 
              then do 
                  assertOr [ not x, not y ]
                  return ( i+j, [] )
              else do 
                  z <- and [ x, y ]
                  return ( i+j , [z] ) 

reduce ( ( x:y:z:ps) : qss ) = do
    ( r, c ) <- full_adder x y z
    qss' <- plugin c qss
    reduce $ ( ps ++ [r] ) : qss' 
reduce ( ( x:y:[]) : qss ) = do
    ( r, c ) <- half_adder x y 
    qss' <- plugin c qss
    reduce $ [r] : qss' 
reduce ( ( x:[]) : qss ) = do
    xs <- reduce qss
    return $ x : xs
reduce [] = return []

plugin c [] = do
    assertOr [ not c ]
    return []
plugin c (qs : qss) = 
    return ((c:qs) : qss)

