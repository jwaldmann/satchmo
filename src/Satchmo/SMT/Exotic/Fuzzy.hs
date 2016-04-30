{-# language FlexibleInstances #-}
{-# language FlexibleContexts #-}
{-# language MultiParamTypeClasses #-}

module Satchmo.SMT.Exotic.Fuzzy where

import Satchmo.SMT.Exotic.Dict
import qualified Satchmo.SMT.Exotic.Domain

import qualified Satchmo.Unary.Op.Flexible as X
import qualified Satchmo.Unary as N
import qualified Satchmo.Boolean as B

import Satchmo.Code

import qualified Data.Map as M
import qualified Satchmo.SMT.Exotic.Semiring as S
import qualified Satchmo.SMT.Exotic.Semiring.Fuzzy as F


import Satchmo.SAT.Mini ( SAT)

data Fuzzy = Fuzzy { contents :: N.Number }

minus_infinite f = B.not $ head $ N.bits $ contents f
plus_infinite f = last $ N.bits $ contents f

make = \ c -> do
    return $ Fuzzy { contents = c }


instance ( Decode m B.Boolean Bool, Decode m N.Number Integer )
         => Decode m Fuzzy ( F.Fuzzy Integer ) where
    decode a = do
        p <- decode $ plus_infinite a
        c <- decode $ contents a
        m <- decode $ minus_infinite a
        return $ if p then F.Plus_Infinite 
                 else if m then F.Minus_Infinite
                 else F.Finite c

dict :: Int -> Dict SAT Fuzzy B.Boolean
dict bits = Dict { domain = Satchmo.SMT.Exotic.Domain.Fuzzy 
  , fresh = do
    c <- N.number bits
    make c
  , finite = \ x -> return $ B.not $ plus_infinite x
  , ge = \ l r ->
    B.monadic B.or [ return $ plus_infinite l
                   , return $ minus_infinite r
                  , N.gt ( contents l ) ( contents r ) 
                 ]
  , gg = \ l r ->
    B.monadic B.or [ return $ plus_infinite l
                  , N.gt ( contents l ) ( contents r ) 
                 ]
  , plus = \ xs -> do -- min
    c <- X.minimum $ map contents xs
    make c
  , times = \ xs -> do -- max
    c <- X.maximum $ map contents xs
    make c
  }
