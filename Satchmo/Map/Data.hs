{-# language FlexibleInstances, MultiParamTypeClasses, FlexibleContexts #-}
{-# language TupleSections #-}

module Satchmo.Map.Data

( Map
, unknown, constant
, (!), 
) 

where

import Satchmo.Code
import qualified Satchmo.Boolean as B

import Satchmo.SAT

import qualified Data.Set as S
import qualified Data.Map.Strict as M

import Control.Monad ( guard, forM )
import Control.Applicative ( (<$>), (<*>) )

newtype Map a b = Map (M.Map a b)

Map m ! i = m M.! i

instance ( Functor m, Decode m b c, Ord a )
         => Decode m (Map a b) ( M.Map a c) where
    decode (Map m) = decode m

-- | allocate an unknown map with this domain
unknown :: ( B.MonadSAT m , Ord a )
         => [a] -> m b -> m (Map a b)
unknown xs build = Map <$> M.fromList 
     <$> ( forM xs $ \ x -> (x,) <$> build )

constant :: ( B.MonadSAT m , Ord a )
         => [(a,c)] -> (c -> m b) -> m (Map a b)
constant xys encode = Map <$> M.fromList 
     <$> ( forM xys $ \ (x,y) -> (x,) <$> encode y )


