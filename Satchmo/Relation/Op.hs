{-# language FlexibleInstances, MultiParamTypeClasses #-}

module Satchmo.Relation.Op

( mirror
, union
, complement
, product
, intersection
) 

where

import Prelude hiding ( and, or, not, product )
import qualified Prelude

import Satchmo.Code
import Satchmo.Boolean
import Satchmo.Counting
import Satchmo.Relation.Data

import Control.Monad ( guard )
import Data.Ix

import Satchmo.SAT

mirror :: ( Ix a , Ix b ) => Relation a b -> Relation b a
mirror r = 
    let ((a,b),(c,d)) = bounds r
    in  build ((b,a),(d,c)) $ do (x,y) <- indices r ; return ((y,x), r!(x,y))

complement :: ( Ix a , Ix b ) => Relation a b -> Relation a b
complement r = 
    build (bounds r) $ do i <- indices r ; return ( i, not $ r!i )

union :: ( Ix a , Ix b, MonadSAT m ) 
      => Relation a b -> Relation a b 
      -> m ( Relation a b )
{-# specialize inline union :: ( Ix a , Ix b ) => Relation a b -> Relation a b -> SAT ( Relation a b ) #-}      
union r s = do
    pairs <- sequence $ do
        i <- indices r
        return $ do o <- or [ r!i, s!i ] ; return ( i, o )
    return $ build ( bounds r ) pairs

product :: ( Ix a , Ix b, Ix c, MonadSAT m ) 
        => Relation a b -> Relation b c -> m ( Relation a c )
{-# specialize inline product ::  ( Ix a , Ix b, Ix c ) => Relation a b -> Relation b c -> SAT ( Relation a c ) #-}      
product a b = do
    let ((ao,al),(au,ar)) = bounds a
        ((bo,bl),(bu,br)) = bounds b
        bnd = ((ao,bl),(au,br))
    pairs <- sequence $ do
        i @ (x,z) <- range bnd
        return $ do
            o <- monadic or $ do
                y <- range ( al, ar )
                return $ and [ a!(x,y), b!(y,z) ]
            return ( i, o )
    return $ build bnd pairs

intersection :: ( Ix a , Ix b, MonadSAT m ) 
      => Relation a b -> Relation a b 
      -> m ( Relation a b )
{-# specialize inline intersection ::  ( Ix a , Ix b ) => Relation a b -> Relation a b -> SAT ( Relation a b ) #-} 
intersection r s = do
    pairs <- sequence $ do
        i <- indices r
        return $ do a <- and [ r!i, s!i ] ; return ( i, a )
    return $ build ( bounds r ) pairs


