module Satchmo.Relation.Prop

( implies
, symmetric 
, transitive
, irreflexive
, reflexive
, regular
)

where

import Prelude hiding ( and, or, not, product )
import qualified Prelude

import Satchmo.Code
import Satchmo.Boolean
import Satchmo.Counting
import Satchmo.Relation.Data
import Satchmo.Relation.Op

import Control.Monad ( guard )
import Data.Ix

implies :: ( Ix a, Ix b, MonadSAT m ) 
        => Relation a b -> Relation a b -> m Boolean
implies r s = monadic and $ do
    i <- indices r
    return $ or [ not $ r ! i, s ! i ]


symmetric :: ( Ix a, MonadSAT m) => Relation a a -> m Boolean
symmetric r = implies r ( mirror r )

irreflexive :: ( Ix a, MonadSAT m) => Relation a a -> m Boolean
irreflexive r = and $ do
    let ((a,b),(c,d)) = bounds r
    x <- range ( a, c)
    return $ Satchmo.Boolean.not $ r ! (x,x) 

reflexive :: ( Ix a, MonadSAT m) => Relation a a -> m Boolean
reflexive r = and $ do
    let ((a,b),(c,d)) = bounds r
    x <- range (a,c)
    return $ r ! (x,x) 

regular :: ( Ix a, MonadSAT m) => Int -> Relation a a -> m Boolean
regular deg r = monadic and $ do
    let ((a,b),(c,d)) = bounds r
    x <- range ( a , c )
    return $ exactly deg $ do 
        y <- range (b,d)
        return $ r !(x,y)

transitive :: ( Ix a, MonadSAT m ) 
           => Relation a a -> m Boolean
transitive r = do
    r2 <- product r r
    implies r2 r
