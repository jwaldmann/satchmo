-- | fixed bit width tropical numbers,
-- table lookup for ring multiplication

{-# language FlexibleInstances #-}
{-# language FlexibleContexts #-}
{-# language MultiParamTypeClasses #-}

module Satchmo.SMT.Exotic.Tropical where

import Satchmo.SMT.Exotic.Dict
import qualified Satchmo.SMT.Exotic.Domain

import qualified Data.Map as M

-- see below (implementation of "times") for switching to Fixed
-- import qualified Satchmo.Unary.Op.Flexible as X
import qualified Satchmo.Unary.Op.Fixed as X
import qualified Satchmo.Unary as N

import qualified Satchmo.Boolean as B

import Satchmo.Code
import Satchmo.SAT.Mini (SAT)


import Control.Monad ( foldM, forM, guard, when )

import qualified Satchmo.SMT.Exotic.Semiring.Tropical as T


data Tropical = Tropical { contents :: N.Number }

plus_infinite = last . N.bits . contents

instance ( Decode m B.Boolean Bool )
         => Decode m Tropical ( T.Tropical Integer ) where
    decode a = do
        p <- decode $ plus_infinite a
        c <- decode $ contents a
        return $ if p then T.Plus_Infinite else T.Finite c

make c = do
    return $ Tropical { contents = c }

dict :: Int 
     -> Dict SAT  Tropical B.Boolean
dict bits = Dict { domain = Satchmo.SMT.Exotic.Domain.Tropical 
  , fresh = do
    c <- N.number bits
    make c
  , finite = \ x -> return $ B.not $ plus_infinite x
  , ge = \ l r -> N.ge ( contents l ) ( contents r ) 
  , gg = \ l r ->
    B.monadic B.or [ return $ plus_infinite l
                   , N.gt ( contents l ) ( contents r ) 
                   ]
  , plus = \ xs -> do 
    c <- X.minimum $ for xs contents
    make c
  , times = \ [s,t] -> do
          p <- B.or [ plus_infinite s, plus_infinite t ]
          let a = contents s ; b = contents t
          let width = length $ N.bits a
          when ( length ( N.bits b ) /= width ) 
               $ error "Tropical.times: different bit widths"
          t <- B.constant True
          pairs <- sequence $ do
              (i,x) <- zip [0 .. ] $ t : N.bits a
              (j,y) <- zip [0 .. ] $ t : N.bits b
              guard $ i+j > 0
              guard $ i+j <= width
              return $ do z <- B.and [x,y] ; return (i+j, [z])
          cs <- forM ( map snd $ M.toAscList $ M.fromListWith (++) pairs ) B.or
          -- if result is not plus_inf, then overflow is not allowed
          B.assert [ p , B.not $ last cs ]
          make $ N.make cs
  }


for = flip map
