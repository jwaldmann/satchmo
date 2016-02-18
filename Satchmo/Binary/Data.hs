{-# language MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, UndecidableInstances #-}


module Satchmo.Binary.Data

( Number, bits, make
, width, number, constant, constantWidth
, fromBinary, toBinary, toBinaryWidth
)

where

import Prelude hiding ( and, or, not )

import qualified Satchmo.Code as C

import Satchmo.Boolean hiding ( constant )
import qualified  Satchmo.Boolean as B

import Satchmo.Counting

data Number = Number 
            { bits :: [ Boolean ] -- lsb first
            }

instance (Monad m, C.Decode m Boolean Bool) => C.Decode m Number Integer where
    decode n = do ys <- mapM C.decode (bits n) ; return $ fromBinary ys

width :: Number -> Int
width n = length $ bits n

-- | declare a number variable (bit width)
number :: MonadSAT m => Int -> m Number
number w = do
    xs <- sequence $ replicate w boolean
    return $ make xs

make :: [ Boolean ] -> Number
make xs = Number
           { bits = xs
           }

fromBinary :: [ Bool ] -> Integer
fromBinary xs = foldr ( \ x y -> 2*y + if x then 1 else 0 ) 0 xs

toBinary :: Integer -> [ Bool ]
toBinary 0 = []
toBinary n  = 
    let (d,m) = divMod n 2
    in  toEnum ( fromIntegral m ) : toBinary d

-- | @toBinaryWidth w@ converts to binary using at least @w@ bits
toBinaryWidth :: Int -> Integer -> [Bool]
toBinaryWidth width n =
    let bs = toBinary n
        leadingZeros = max 0 $ width - (length bs)
    in
      bs ++ (replicate leadingZeros False)

-- | Declare a number constant 
constant :: MonadSAT m => Integer -> m Number
constant n = do
    xs <- mapM B.constant $ toBinary n
    return $ make xs

-- | @constantWidth w@ declares a number constant using at least @w@ bits
constantWidth :: MonadSAT m => Int -> Integer -> m Number
constantWidth width n = do
  xs <- mapM B.constant $ toBinaryWidth width n
  return $ make xs
