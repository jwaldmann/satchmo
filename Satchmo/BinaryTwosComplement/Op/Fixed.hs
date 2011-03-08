{-# language MultiParamTypeClasses #-}

-- | Operations with fixed bit width.
-- Still they are non-overflowing:
-- if overflow occurs, the constraints are not satisfiable.
-- The bit width of the result of binary operations
-- is the max of the bit width of the inputs.

module Satchmo.BinaryTwosComplement.Op.Fixed
    ( add, times, increment, negate
    , module Satchmo.BinaryTwosComplement.Data
    , module Satchmo.BinaryTwosComplement.Op.Common
    )
where

import Prelude hiding (not,negate)
import Control.Applicative ((<$>))
import Satchmo.MonadSAT (MonadSAT)
import Satchmo.BinaryTwosComplement.Op.Common
import Satchmo.BinaryTwosComplement.Data
import qualified Satchmo.Binary.Op.Common as C
import qualified Satchmo.Binary.Op.Flexible as F
import Satchmo.Boolean (monadic,assertOr,equals2,implies,not)
import qualified Satchmo.Boolean as Boolean

import Control.Monad.Trans (liftIO)

-- Sign extension
extendMsb :: Number -> Number
extendMsb n = fromBooleans $ bits n ++ [msb n]

add :: (MonadSAT m) => Number -> Number -> m Number
add a b = do
  let maxWidth = max (width a) (width b)
      a' = extendMsb a
      b' = extendMsb b

  flexibleResult <- fromUnsigned <$> F.add (toUnsigned a') (toUnsigned b')
  let (low, high) = splitAt maxWidth $ bits flexibleResult
          
  e <- Boolean.equals (last low : high)
  assertOr [ e ]
  return $ fromBooleans low

times :: (MonadSAT m) => Number -> Number -> m Number
times a b = do
  let maxWidth = max (width a) (width b)

  flexibleResult <- fromUnsigned <$> F.times (toUnsigned a) (toUnsigned b)
  let (low, high) = splitAt maxWidth $ bits flexibleResult
          
  e <- Boolean.equals [last low, head high]
  assertOr [ e ]
  return $ fromBooleans low

increment :: (MonadSAT m) => Number -> m Number
increment n =
    let inc [] z = return ( [], z )
        inc (y:ys) z = do
          ( r, c ) <- C.half_adder y z
          ( rAll, cAll ) <- inc ys c
          return ( r : rAll, cAll )
    in do
      add1 <- Boolean.constant True
      (n', _) <- inc (bits n) add1
      e <- (not $ msb n) `implies` (not $ last n')
      assertOr [ e ]
      return $ fromBooleans n'

negate :: (MonadSAT m) => Number -> m Number
negate n =
    let invN = fromBooleans $ map not $ bits n
    in do
      n' <- increment invN
      e <- (msb n) `implies` (not $ msb n')
      assertOr [ e ]
      return n'
      