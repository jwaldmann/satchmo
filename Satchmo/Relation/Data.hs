{-# language FlexibleInstances, MultiParamTypeClasses #-}

module Satchmo.Relation.Data

( Relation, relation, build
, bounds, (!), indices
, table
) 

where

import Satchmo.Code
import Satchmo.Boolean

import qualified Data.Array as A
import Data.Array hiding ( bounds, (!), indices )

import Control.Monad ( guard )

data Relation a b = Relation ( Array (a,b) Boolean ) 

relation :: ( Ix a, Ix b, MonadSAT m ) 
         => ((a,b),(a,b)) -> m ( Relation a b ) 
relation bnd = do
    pairs <- sequence $ do 
        p <- range bnd
        return $ do
            x <- boolean
            return ( p, x )
    return $ build bnd pairs

build :: ( Ix a, Ix b ) 
      => ((a,b),(a,b)) 
      -> [ ((a,b), Boolean ) ]
      -> Relation a b 
build bnd pairs = Relation $ array bnd pairs

bounds :: (Ix a, Ix b) => Relation a b -> ((a,b),(a,b))
bounds ( Relation r ) = A.bounds r

indices ( Relation r ) = A.indices r

Relation r ! p = r A.! p

instance (Ix a, Ix b) => Decode ( Relation a b ) ( Array (a,b) Bool ) where
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


