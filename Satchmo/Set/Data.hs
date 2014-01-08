{-# language FlexibleInstances, MultiParamTypeClasses, FlexibleContexts #-}
{-# language TupleSections #-}

module Satchmo.Set.Data

( Set , unknown, constant
, member, keys, keysSet, keys, assocs, elems
, all2, common2
) 

where

import Satchmo.Code
import qualified Satchmo.Boolean as B

import Satchmo.SAT

import qualified Data.Set as S
import qualified Data.Map.Strict as M

import Satchmo.Map

import Control.Monad ( guard, forM )
import Control.Applicative ( (<$>), (<*>) )

newtype Set a = Set (M.Map a B.Boolean)

instance ( Functor m, Decode m B.Boolean Bool, Ord a )
         => Decode m (Set a) ( S.Set a) where
    decode (Set m) = 
        M.keysSet <$> M.filter id <$> decode m

keys (Set m) = M.keys m
keysSet (Set m) = M.keysSet m
assocs (Set m) = M.assocs m
elems (Set m) = M.elems m

member x (Set m) = case M.lookup x m of
    Nothing -> B.constant False
    Just y  -> return y


-- | allocate an unknown subset of these elements
unknown :: ( B.MonadSAT m , Ord a )
         => [a] -> m (Set a)
unknown xs = Set <$> M.fromList 
     <$> ( forM xs $ \ x -> (x,) <$> B.boolean )

constant :: ( B.MonadSAT m , Ord a )
         => [a] -> m (Set a)
constant xs = Set <$> M.fromList 
     <$> ( forM xs $ \ x -> (x,) <$> B.constant True )

all2 f s t = B.and
 =<< forM ( S.toList $ S.union (keysSet s)(keysSet t))
 ( \ x -> do a <- member x s; b <- member x t; f a b )

common2 f s t = Set <$> M.fromList <$>
 forM ( S.toList $ S.union (keysSet s)(keysSet t))
 ( \ x -> do a <- member x s; b <- member x t
             y <- f a b ; return (x,y) )

