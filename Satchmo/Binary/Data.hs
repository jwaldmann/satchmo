{-# language MultiParamTypeClasses #-}

module Satchmo.Binary.Data

( Number, bits, make
, width, number, constant
)

where

import Prelude hiding ( and, or, not )

import qualified Satchmo.Code as C

import Satchmo.Boolean hiding ( constant )
import qualified  Satchmo.Boolean as B

import Satchmo.Counting

data Number = Number 
            { bits :: [ Boolean ] -- lsb first
            , decode :: C.Decoder Integer
            }

instance C.Decode Number Integer where
    decode = decode

width :: Number -> Int
width n = length $ bits n

-- | declare a number variable (bit width)
number :: Int -> SAT Number
number w = do
    xs <- sequence $ replicate w boolean
    return $ make xs

make :: [ Boolean ] -> Number
make xs = Number
           { bits = xs
           , decode = do ys <- mapM C.decode xs ; return $ fromBinary ys
           }

fromBinary :: [ Bool ] -> Integer
fromBinary xs = foldr ( \ x y -> 2*y + if x then 1 else 0 ) 0 xs

toBinary :: Integer -> [ Bool ]
toBinary 0 = []
toBinary n  = 
    let (d,m) = divMod n 2
    in  toEnum ( fromIntegral m ) : toBinary d

-- | declare a number constant 
constant :: Integer -> SAT Number
constant n = do
    xs <- mapM B.constant $ toBinary n
    return $ make xs

