{-# language MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, UndecidableInstances #-}

module Satchmo.BinaryTwosComplement.Data
    ( Number, bits, fromBooleans, number, toUnsigned, fromUnsigned
    , width, isNull, msb, constant, constantWidth)

where

import Control.Applicative ((<$>))
import Satchmo.MonadSAT (MonadSAT)
import Satchmo.Boolean (Boolean)
import qualified Satchmo.Boolean as Boolean
import qualified Satchmo.Code as C
import qualified Satchmo.Binary.Data as B 

import Debug.Trace

data Number = Number 
            { bits :: [Boolean] -- LSB first
            }


instance (Monad m, C.Decode m Boolean Bool) => C.Decode m Number Integer where
    decode n = do bs <- C.decode $ bits n ; return $ fromBinary bs

-- | Make a number from its binary representation
fromBooleans :: [Boolean] -> Number
fromBooleans xs = Number xs


-- | Convert to unsigned number (see "Satchmo.Binary.Op.Flexible")
toUnsigned :: Number -> B.Number
toUnsigned = B.make . bits

-- | Convert from unsigned number (see "Satchmo.Binary.Op.Flexible").
-- The result is interpreted as a positive or negative number,
-- depending on its most significant bit.
fromUnsigned :: B.Number -> Number
fromUnsigned = fromBooleans . B.bits

-- | Get bit width
width :: Number -> Int
width = length . bits

-- | Most significant bit
msb :: Number -> Boolean
msb n = if isNull n then error "Satchmo.BinaryTwosComplement.Data.msb"
        else bits n !! (width n - 1)

-- | @isNull n == True@ if @width n == 0@
isNull :: Number -> Bool
isNull n = width n == 0

-- | Get a number variable of given bit width
number :: MonadSAT m => Int -> m Number
number width = do
  xs <- sequence $ replicate width Boolean.boolean
  return $ fromBooleans xs

fromBinary :: [Bool] -> Integer
fromBinary xs =
    let w = length xs
        (bs, [msb]) = splitAt (w - 1) xs
    in                    
      if msb then -(2^(w-1)) + (B.fromBinary bs)
      else B.fromBinary bs

toBinary :: Maybe Int -- ^ Minimal bit width
         -> Integer -> [Bool]
toBinary width i = 
    let i' = abs i
        binary = maybe (B.toBinary i') (B.toBinaryWidth `flip` i') width
        flipBits (firstOne,result) x =
            if firstOne then (True, result ++ [not x]) 
            else (x, result ++ [x])
    in
      if i == 0 then
          replicate (maybe 1 id width) False
      else if i < 0 then 
               let flipped = snd $ foldl flipBits (False,[]) binary
               in
                 if last flipped == False then flipped ++ [True]
                 else flipped
           else 
               if i > 0 && last binary == True then binary ++ [False]
               else binary

-- | Get a number constant
constant :: MonadSAT m => Integer -> m Number
constant i = do
  bs <- mapM Boolean.constant $ toBinary Nothing i
  return $ fromBooleans bs
    
-- | @constantWidth w@ declares a number constant using at least @w@ bits
constantWidth :: MonadSAT m => Int -> Integer -> m Number
constantWidth width i = do
  bs <- mapM Boolean.constant $ toBinary (Just width) i
  return $ fromBooleans bs
