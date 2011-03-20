{-# language MultiParamTypeClasses #-}

-- | Operations with fixed bit width.
-- Still they are non-overflowing:
-- if overflow occurs, the constraints are not satisfiable.
-- The bit width of the result of binary operations
-- is the max of the bit width of the inputs.

module Satchmo.BinaryTwosComplement.Op.Fixed
    ( add, subtract, times, increment, negate, linear
    , module Satchmo.BinaryTwosComplement.Data
    , module Satchmo.BinaryTwosComplement.Op.Common
    )
where

import Prelude hiding (not,negate, subtract)
import Control.Applicative ((<$>))
import Satchmo.MonadSAT (MonadSAT)
import Satchmo.BinaryTwosComplement.Op.Common
import Satchmo.BinaryTwosComplement.Data
import qualified Satchmo.Binary.Op.Common as C
import qualified Satchmo.Binary.Op.Flexible as F
import Satchmo.Binary.Op.Fixed (restrictedTimes)
import Satchmo.Boolean (Boolean,monadic,assertOr,equals2,implies,not)
import qualified Satchmo.Boolean as Boolean

-- Sign extension
extendMsb :: Int -> Number -> Number
extendMsb i n = fromBooleans $ bits n ++ (replicate i $ msb n)

add :: (MonadSAT m) => Number -> Number -> m Number
add a b = do
  let maxWidth  = max (width a) (width b)
      widthDiff = abs $ (width a) - (width b)
      extend x = if width x == maxWidth then extendMsb 1 x
                 else extendMsb (widthDiff + 1) x
      a' = extend a
      b' = extend b

  flexibleResult <- fromUnsigned <$> F.add (toUnsigned a') (toUnsigned b')
  let (low, high) = splitAt maxWidth $ bits flexibleResult

  e <- Boolean.equals [last low, head high]
  assertOr [ e ]
  return $ fromBooleans low

times :: MonadSAT m => Number -> Number -> m Number
times a b = do
  let a' = extendMsb (width b) a
      b' = extendMsb (width a) b
      unsignedResultWidth = (width a) + (width b)
      resultWidth = max (width a) (width b)

  unsignedResult <- fromUnsigned <$> 
                    restrictedTimes (toUnsigned a') (toUnsigned b')
  let (low, high) = splitAt resultWidth $ bits unsignedResult
  allHighOne  <- Boolean.and $ high
  allHighZero <- Boolean.and $ map not high
  assertOr [allHighOne, allHighZero]

  e <- Boolean.equals [ last low, head high ]
  assertOr [e]
  return $ fromBooleans low

increment :: MonadSAT m => Number -> m Number
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

subtract :: MonadSAT m => Number -> Number -> m Number
subtract a b = do
    b' <- negate b
    add a b'

negate :: MonadSAT m => Number -> m Number
negate n =
    let invN = fromBooleans $ map not $ bits n
    in do
      n' <- increment invN
      e <- (msb n) `implies` (not $ msb n')
      assertOr [ e ]
      return n'
      
linear :: MonadSAT m => Number -> Number -> Number -> m Number
linear m x n = m `times` x >>= add n
