{-# language FlexibleInstances #-}
{-# language FlexibleContexts #-}
{-# language MultiParamTypeClasses #-}

module Satchmo.SMT.Exotic.Arctic where

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


data Arctic = Arctic { contents :: N.Number
                     }

minus_infinite = B.not . head . N.bits . contents

instance ( Decode m B.Boolean Bool )
         => Decode m Arctic ( A.Arctic Integer ) where
    decode a = do
        c <- decode $ contents a
        return $ if 0 == c then A.Minus_Infinite else A.Finite (c-1)

make c = do
    return $ Arctic { contents = c }

dict :: Int 
     -> Dict SAT Arctic B.Boolean
dict bits = Dict { domain = Satchmo.SMT.Exotic.Domain.Arctic 
  , fresh = do
    c <- N.number bits
    make c
  , finite = \ x -> return $ B.not $ minus_infinite x
  , ge = \ l r -> N.ge ( contents l ) ( contents r ) 
  , gg = \ l r ->
    B.monadic B.or [ return $ minus_infinite r
                   , N.gt ( contents l ) ( contents r ) 
                   ]
  , plus = \ xs -> do 
    c <- X.maximum $ map contents xs
    make c
  , times = \ [s,t] -> do
          m <- B.or [ minus_infinite s, minus_infinite t ]
          let a = contents s ; b = contents t
          let width = length $ N.bits a
          when ( length ( N.bits b ) /= width ) 
               $ error "Arctic.times: different bit widths"
          pairs <- sequence $ do
              (i,x) <- zip [0 .. ] $ N.bits a
              (j,y) <- zip [0 .. ] $ N.bits b
              guard $ i+j <= width
              return $ do z <- B.and [x,y] ; return (i+j, [z])
          cs <- forM ( map snd $ M.toAscList $ M.fromListWith (++) pairs ) B.or
          -- overflow is not allowed
          B.assert [ B.not $ last cs ]
          ds <- forM (init cs) $ \ c -> B.and [ B.not m, c ]
          make $ N.make ds
  }

