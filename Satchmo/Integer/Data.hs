{-# language MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, UndecidableInstances #-}

module Satchmo.Integer.Data 

( Number, make, number
, constant, decode
, bits, width, sign
)

where

import Prelude hiding ( and, or, not, (&&), (||) )
import qualified Prelude 

import qualified Satchmo.Code as C

import Satchmo.Boolean hiding ( constant )
import qualified  Satchmo.Boolean as B

import Satchmo.Counting
import Control.Monad

data Number = Number 
            { bits :: [ Boolean ] -- ^ lsb first,
	         -- using two's complement
            }

instance C.Decode m Boolean Bool => C.Decode m Number Integer where
    decode n = do ys <- mapM C.decode (bits n) ; return $ fromBinary ys

width :: Number -> Int
width n = length $ bits n

sign :: Number -> Boolean
sign n = case bits n of
  [] -> error "Satchmo.Integer.Data:sign no bits"
  bs -> last bs

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

-- | declare a number constant 
constant :: MonadSAT m 
	 => Int -- ^ bit width
	 -> Integer -- ^ value
	 -> m Number
constant w n = do
    xs <- if 0 <= n Prelude.&& n < 2^(w-1)
          then mapM B.constant $ toBinary n
	  else if negate ( 2^(w-1)) <= n Prelude.&& n < 0
	  then mapM B.constant $ toBinary (n + 2^w)
	  else error "Satchmo.Integer.Data.constant"
    z <- B.constant False
    return $ make $ take w $ xs ++ repeat z

decode w n = do
  bs <- forM (bits n) C.decode
  return $ fromBinary bs
         - if last bs then 2^w else 0
