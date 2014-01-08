{-# language NoMonomorphismRestriction #-}

module Satchmo.Set.Op where

import Satchmo.Set.Data
import qualified Satchmo.Boolean as B
import qualified Satchmo.Counting as C

import qualified Data.Set as S

import Control.Monad ( guard, forM, liftM2 )
import Control.Applicative ( (<$>), (<*>) )

null s = B.not <$> B.or ( elems s )

equals = all2 B.equals2 
isSubsetOf = all2 $ B.implies

union s t = common2 (B.||)
intersection s t = common2 (B.&&)

difference s t = common2 ( \ x y -> x B.&& (B.not y) )

isSingleton s = C.exactly 1 $ elems s

