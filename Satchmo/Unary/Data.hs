{-# language MultiParamTypeClasses #-}
{-# language FlexibleInstances #-}
{-# language FlexibleContexts #-}
{-# language UndecidableInstances #-}

module Satchmo.Unary.Data 
       
( Number, bits, make       
, width, number, constant )                
       
where

import Prelude hiding ( and, or, not )

import qualified Satchmo.Code as C

import Satchmo.Boolean hiding ( constant )
import qualified  Satchmo.Boolean as B

import Control.Monad ( forM, when )

data Number = Number
            { bits :: [ Boolean ] 
            -- ^ contents is [ 1 .. 1 0 .. 0 ]
            -- number of 1 is value of number  
            }  
            
instance C.Decode m Boolean Bool => C.Decode m Number Int where            
    decode n = do
        bs <- forM ( bits n ) C.decode
        return $ length $ filter id bs

width :: Number -> Int
width n = length $ bits n

-- | declare a number with range (0, w)
number :: MonadSAT m => Int -> m  Number 
number w = do
    xs <- sequence $ replicate w boolean
    forM ( zip xs $ tail xs ) $ \ (p, q) ->
        assert [ p, not q ]
    return $ make xs
    
make :: [ Boolean ] -> Number 
make xs = Number { bits = xs }

constant :: MonadSAT m => Integer -> m Number 
constant k = do
    xs <- forM [ 1 .. k ] $ \ i -> B.constant True
    return $ make xs