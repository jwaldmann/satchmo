{-# language NoMonomorphismRestriction #-}

module Satchmo.Set.Op where

import Satchmo.Set.Data
import qualified Satchmo.Boolean as B
import qualified Satchmo.Counting as C

import qualified Data.Set as S

import Control.Monad ( guard, forM, liftM2 )
import Control.Applicative ( (<$>), (<*>) )

null :: (Ord a, B.MonadSAT m) => Set a -> m B.Boolean
null s = B.not <$> B.or ( elems s )

equals :: (Ord a, B.MonadSAT m) => Set a -> Set a -> m B.Boolean
equals = all2 B.equals2 

isSubsetOf :: (Ord a, B.MonadSAT m) => Set a -> Set a -> m B.Boolean
isSubsetOf = all2 $ B.implies

isSingleton :: (Ord a, B.MonadSAT m) => Set a -> m B.Boolean
isSingleton s = C.exactly 1 $ elems s

union :: (Ord a, B.MonadSAT m) => Set a -> Set a -> m (Set a)
union = common2 (B.||) 

intersection :: (Ord a, B.MonadSAT m) => Set a -> Set a -> m (Set a)
intersection = common2 (B.&&)

difference :: (Ord a, B.MonadSAT m) => Set a -> Set a -> m (Set a)
difference = common2 ( \ x y -> x B.&& (B.not y) )



