{-# language FlexibleInstances, MultiParamTypeClasses, FlexibleContexts #-}

module Satchmo.Relation.Data

( Relation
, relation, symmetric_relation
, build
, identity                      
, bounds, (!), indices, assocs, elems
, table
) 

where

import Satchmo.Code
import Satchmo.Boolean

import Satchmo.SAT

import qualified Data.Array as A
import Data.Array ( Array, Ix )
import Data.Functor ((<$>))

import Control.Monad ( guard, forM )

newtype Relation a b = Relation ( Array (a,b) Boolean ) 

relation :: ( Ix a, Ix b, MonadSAT m ) 
         => ((a,b),(a,b)) -> m ( Relation a b ) 
{-# specialize inline relation :: ( Ix a, Ix b) => ((a,b),(a,b)) -> SAT ( Relation a b ) #-} 
relation bnd = do
    pairs <- sequence $ do 
        p <- A.range bnd
        return $ do
            x <- boolean
            return ( p, x )
    return $ build bnd pairs
    
symmetric_relation bnd = do
    pairs <- sequence $ do
        (p,q) <- A.range bnd
        guard $ p <= q
        return $ do
            x <- boolean
            return $ [ ((p,q), x ) ]
                   ++ [ ((q,p), x) | p /= q ]
    return $ build bnd $ concat pairs          

identity :: ( Ix a, MonadSAT m) 
         => ((a,a),(a,a)) -> m ( Relation a a )
identity bnd = do            
    f <- constant False
    t <- constant True
    return $ build bnd $ for ( A.range bnd ) $ \ (i,j) ->
        ((i,j), if i == j then t else f )

for = flip map

build :: ( Ix a, Ix b ) 
      => ((a,b),(a,b)) 
      -> [ ((a,b), Boolean ) ]
      -> Relation a b 
build bnd pairs = Relation $ A.array bnd pairs


bounds :: (Ix a, Ix b) => Relation a b -> ((a,b),(a,b))
bounds ( Relation r ) = A.bounds r

indices ( Relation r ) = A.indices r

assocs ( Relation r ) = A.assocs r

elems ( Relation r ) = A.elems r

Relation r ! p = r A.! p

instance (Ix a, Ix b, Decode m Boolean Bool) 
    => Decode m  ( Relation a b ) ( Array (a,b) Bool ) where
    decode ( Relation r ) = do
        decode r

table :: (Enum a, Ix a, Enum b, Ix b) 
      => Array (a,b) Bool -> String
table r = unlines $ do
    let ((a,b),(c,d)) = A.bounds r
    x <- [ a .. c ]
    return $ unwords $ do
        y <- [ b .. d ]
        return $ if r A.! (x,y) then "*" else "."


