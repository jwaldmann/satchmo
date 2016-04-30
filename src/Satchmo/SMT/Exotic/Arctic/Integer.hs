{-# language FlexibleInstances #-}
{-# language FlexibleContexts #-}
{-# language MultiParamTypeClasses #-}

module Satchmo.SMT.Exotic.Arctic.Integer where

import Satchmo.SMT.Exotic.Dict
import qualified Satchmo.SMT.Exotic.Domain

import qualified Data.Map as M

import qualified Satchmo.Unary.Op.Flexible as X
import qualified Satchmo.Unary as N
import qualified Satchmo.Boolean as B

import Satchmo.Code
import Satchmo.SAT.Mini (SAT)
import Control.Monad (forM, guard, when)

import qualified Satchmo.SMT.Exotic.Semiring.Arctic as A

-- | (contents a !! shift a) == (number is > 0)
-- (contents a !! 0) == (number is > -infty)
-- (so Arctic Natural has shift = 1)
data Arctic = Arctic { contents :: N.Number
                     , shift :: Int  
                     }

minus_infinite = B.not . head . N.bits . contents
not_minus_infinite = head . N.bits . contents


instance ( Decode m B.Boolean Bool )
         => Decode m Arctic ( A.Arctic Integer ) where
    decode a = do
        c <- decode $ contents a
        return $ if 0 == c 
                 then A.Minus_Infinite 
                 else A.Finite $ fromIntegral (c - shift a)

constant :: Int -> A.Arctic Integer 
         -> SAT Arctic
constant bits a = case a of
    A.Minus_Infinite -> do
       cs <- forM [ negate bits + 1 .. bits ] $ \ _ -> 
                B.constant False
       make bits $ N.make cs
    A.Finite f -> do
       cs <- forM [ negate bits + 1 .. bits ] $ \ i -> 
                B.constant ( i <= fromIntegral f )
       make bits $ N.make cs

make s c = do
    return $ Arctic { contents = c, shift = s }

dict :: Int 
     -> Dict SAT Arctic B.Boolean
dict bits = Dict 
  { domain = Satchmo.SMT.Exotic.Domain.Arctic 
  , fresh = do
    c <- N.number $ 2 * bits
    make bits c
  -- actually the following should be called "positive" 
  -- by which we mean ">= 0"  
  , finite = \ x -> 
      return $ N.bits (contents x) !! (shift x - 1)
  , ge = \ l r -> N.ge ( contents l ) ( contents r ) 
  , gg = \ l r ->
    B.monadic B.or [ return $ minus_infinite r
                   , N.gt ( contents l ) ( contents r ) 
                   ]
  , plus = \ xs -> do 
    c <- X.maximum $ map contents xs
    make bits c
  , times = \ [s,t] -> do
          m <- B.or [ minus_infinite s
                    , minus_infinite t ]
          let a = contents s ; b = contents t
          pairs <- sequence $ do
              (i,x) <- zip [negate bits + 1 .. ] $ N.bits a
              (j,y) <- zip [negate bits + 1 .. ] $ N.bits b
              guard $ i+j > negate bits 
              guard $ i+j <= bits
              return $ do 
                  z <- B.and [x,y, B.not m] 
                  return (i+j, [z])
          cs <- forM ( map snd $ M.toAscList 
                     $ M.fromListWith (++) pairs ) B.or
          B.assert [ m, head cs ] -- no underflow      
          B.assert [ B.not $ last cs ] -- no overflow
          make bits $ N.make $ init cs
  }

