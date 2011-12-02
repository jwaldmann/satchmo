module Satchmo.Relation.Prop

( implies
, symmetric 
, transitive
, irreflexive
, reflexive
, regular
, empty
)

where

import Prelude hiding ( and, or, not, product )
import qualified Prelude

import Satchmo.Code
import Satchmo.Boolean hiding (implies)
import Satchmo.Counting
import Satchmo.Relation.Data
import Satchmo.Relation.Op

import Control.Monad ( guard )
import Data.Ix

import Satchmo.SAT

implies :: ( Ix a, Ix b, MonadSAT m ) 
        => Relation a b -> Relation a b -> m Boolean
{-# specialize inline implies :: ( Ix a, Ix b ) => Relation a b -> Relation a b -> SAT Boolean #-}      
implies r s = monadic and $ do
    i <- indices r
    return $ or [ not $ r ! i, s ! i ]

empty ::  ( Ix a, Ix b, MonadSAT m ) 
        => Relation a b -> m Boolean
empty r = and $ do
    i <- indices r
    return $ not $ r ! i

symmetric :: ( Ix a, MonadSAT m) => Relation a a -> m Boolean
{-# specialize inline symmetric :: ( Ix a ) => Relation a a -> SAT Boolean #-}      
symmetric r = implies r ( mirror r )

irreflexive :: ( Ix a, MonadSAT m) => Relation a a -> m Boolean
{-# specialize inline irreflexive :: ( Ix a ) =>  Relation a a -> SAT Boolean #-}      
irreflexive r = and $ do
    let ((a,b),(c,d)) = bounds r
    x <- range ( a, c)
    return $ Satchmo.Boolean.not $ r ! (x,x) 

reflexive :: ( Ix a, MonadSAT m) => Relation a a -> m Boolean
{-# specialize inline reflexive :: ( Ix a ) => Relation a a -> SAT Boolean #-}      
reflexive r = and $ do
    let ((a,b),(c,d)) = bounds r
    x <- range (a,c)
    return $ r ! (x,x) 

regular :: ( Ix a, MonadSAT m) => Int -> Relation a a -> m Boolean
{-# specialize inline regular :: ( Ix a ) => Int -> Relation a a -> SAT Boolean #-}      
regular deg r = monadic and $ do
    let ((a,b),(c,d)) = bounds r
    x <- range ( a , c )
    return $ exactly deg $ do 
        y <- range (b,d)
        return $ r !(x,y)

transitive :: ( Ix a, MonadSAT m ) 
           => Relation a a -> m Boolean
{-# specialize inline transitive :: ( Ix a ) => Relation a a -> SAT Boolean #-}      
transitive r = do
    r2 <- product r r
    implies r2 r
