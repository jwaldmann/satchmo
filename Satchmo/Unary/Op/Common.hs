{-# language NoMonomorphismRestriction #-}
{-# language PatternSignatures #-}

module Satchmo.Unary.Op.Common 
       
( iszero, equals
, lt, le, ge, eq, gt
, min, max
)          
       
where


import Prelude 
  hiding ( and, or, not, compare, min, max )
import qualified Prelude

import qualified Satchmo.Code as C

import Satchmo.Unary.Data 
    (Number, make, bits, constant)

import Satchmo.Boolean (MonadSAT, Boolean, Booleans, fun2, fun3, and, or, not, xor, assert, boolean, monadic)
import qualified  Satchmo.Boolean as B

import Control.Monad ( forM, when )

iszero n = case bits n of
    [] -> B.constant True
    x : xs -> return $ not x
    
extended :: MonadSAT m 
         => ( [(Boolean,Boolean)] -> m a )
         -> Number -> Number
         -> m a
extended action a b = do
    f <- B.constant False
    let zipf [] [] = []
        zipf (x:xs) [] = (x,f) : zipf xs []
        zipf [] (y:ys) = (f,y) : zipf [] ys
        zipf (x:xs) (y:ys) = (x,y) : zipf xs ys
    action $ zipf ( bits a ) ( bits b )    
        

le, ge, eq, equals, gt, lt 
  :: MonadSAT m => Number -> Number -> m Boolean

for = flip map

equals = extended $ \ xys -> monadic and $ 
    for xys $ \ (x,y) -> fun2 (==) x y

le = extended $ \ xys -> monadic and $ 
    for xys $ \ (x,y) -> fun2 (<=) x y

ge = flip le

eq = equals

lt a b = fmap not $ ge a b

gt = flip lt

min a b = do 
    cs <- extended ( \ xys -> 
        forM xys $ \ (x,y) -> and [x,y] ) a b
    return $ make cs                              
                          
max a b = do
    cs <- extended ( \ xys -> 
        forM xys $ \ (x,y) -> or [x,y] ) a b
    return $ make cs                      