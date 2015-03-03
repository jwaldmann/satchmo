{-# language TupleSections #-}
{-# language FlexibleInstances #-}
{-# language MultiParamTypeClasses #-}

module Satchmo.Array

( Array
, unknown, constant
, (!), elems, indices, bounds, range, assocs
)
       
where

import Satchmo.Code as C
  
import qualified Data.Array as A
import Control.Applicative
import Control.Monad ( forM )

newtype Array i v = Array (A.Array i v)

unknown bnd build = 
  Array <$> A.array bnd <$> forM (A.range bnd) ( \ i ->
    (i,) <$> build )

constant a = Array a

instance (Functor m, A.Ix i, Decode m c d )
         => Decode m (Array i c) (A.Array i d) where
  decode (Array a) = A.array (A.bounds a) <$> 
    forM (A.assocs a) ( \(k,v) -> (k,) <$> decode v )

Array a ! i = a A.! i
elems (Array a) = A.elems a
indices (Array a) = A.indices a
bounds (Array a) = A.bounds a
range bnd = A.range bnd
assocs (Array a) = A.assocs a
