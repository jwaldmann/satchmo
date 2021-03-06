{-# language MultiParamTypeClasses, PatternGuards #-}

-- | operations from this module cannot overflow.
-- instead they increase the bit width.

module Satchmo.Binary.Op.Flexible

( add, times, dot_product
, add_with_carry, times1, shift
, module Satchmo.Binary.Data
, module Satchmo.Binary.Op.Common
)

where

import Prelude hiding ( and, or, not )

import Satchmo.Boolean
import qualified Satchmo.Code as C
import Satchmo.Binary.Data
import Satchmo.Binary.Op.Common
import qualified Satchmo.Binary.Op.Times as T
import Satchmo.Counting.Unary

import qualified Data.Map as M

add :: (MonadSAT m) => Number -> Number -> m Number
add a b = do
    false <- Satchmo.Boolean.constant False
    ( zs, carry ) <- 
        add_with_carry false (bits a) (bits b)
    return $ make $ zs ++ [carry]

add_with_carry :: (MonadSAT m) => Boolean 
               -> Booleans -> Booleans
               -> m ( Booleans, Boolean )
add_with_carry cin [] [] = return ( [], cin )
add_with_carry cin (x:xs) [] = do
    (z, c) <- half_adder cin x
    ( zs, cout ) <- add_with_carry c xs []
    return ( z : zs, cout )
add_with_carry cin [] (y:ys) = do
    add_with_carry cin (y:ys) []
add_with_carry cin (x:xs ) (y:ys) = do
    (z, c) <- full_adder cin x y
    ( zs, cout ) <- add_with_carry c xs ys
    return ( z : zs, cout )

times :: (MonadSAT m) => Number -> Number -> m Number
times = -- plain_times 
      T.times Nothing

dot_product :: (MonadSAT m) 
             => [ Number ] -> [ Number ] -> m Number
dot_product = T.dot_product Nothing

plain_times :: (MonadSAT m) => Number -> Number -> m Number
plain_times a b | [] <- bits a = return a
plain_times a b | [] <- bits b = return b
plain_times a b | [x] <- bits a = times1 x b
plain_times a b | [y] <- bits b = times1 y a
plain_times a b | x:xs <- bits a = do
    xys  <- times1 x b
    xsys <- plain_times (make xs) b
    zs <- shift xsys
    add xys zs

-- | multiply by 2
shift :: (MonadSAT m) => Number -> m Number
shift a = do
    false <- Satchmo.Boolean.constant False 
    return $ make $ false : bits a

times1 :: (MonadSAT m) => Boolean -> Number -> m Number
times1 x b = do
    zs <- mapM ( \ y -> and [x,y] ) $ bits b
    return $ make zs


