{-# language MultiParamTypeClasses, PatternGuards #-}

-- | operations from this module cannot overflow.
-- instead they increase the bit width.

module Satchmo.Binary.Op.Flexible

( add, times
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
import Satchmo.Counting

add :: Number -> Number -> SAT Number
add a b = do
    false <- Satchmo.Boolean.constant False
    ( zs, carry ) <- add_with_carry false (bits a) (bits b)
    return $ make $ zs ++ [carry]

add_with_carry :: Boolean 
               -> Booleans -> Booleans
               -> SAT ( Booleans, Boolean )
add_with_carry cin [] [] = return ( [], cin )
add_with_carry cin (x:xs) [] = do
    -- z <- xor [ cin, x ]
    -- c <- and [ cin, x ]
    (z, c) <- half_adder cin x
    ( zs, cout ) <- add_with_carry c xs []
    return ( z : zs, cout )
add_with_carry cin [] (y:ys) = do
    add_with_carry cin (y:ys) []
add_with_carry cin (x:xs ) (y:ys) = do
    -- z  <- xor [ cin, x, y ]
    -- c <- atleast 2 [ cin, x, y ]
    (z, c) <- full_adder cin x y
    ( zs, cout ) <- add_with_carry c xs ys
    return ( z : zs, cout )

times :: Number -> Number -> SAT Number
times a b | [x] <- bits a = times1 x b
times a b | x:xs <- bits a = do
    xys  <- times1 x b
    xsys <- times (make xs) b
    zs <- shift xsys
    add xys zs

-- | multiply by 2
shift :: Number -> SAT Number
shift a = do
    false <- Satchmo.Boolean.constant False 
    return $ make $ false : bits a

times1 :: Boolean -> Number -> SAT Number
times1 x b = do
    zs <- mapM ( \ y -> and [x,y] ) $ bits b
    return $ make zs

