
module Satchmo.Relation.Prop

( implies
, symmetric 
, transitive
, irreflexive
, reflexive
, regular
, regular_in_degree
, regular_out_degree
, max_in_degree
, min_in_degree
, max_out_degree
, min_out_degree
, empty
, complete
, disjoint
, equals
, is_function
, is_partial_function
, is_bijection
, is_permutation
)

where

import Prelude hiding ( and, or, not, product )
import qualified Prelude

import Satchmo.Code
import Satchmo.Boolean hiding (implies, equals)
import Satchmo.Counting
import Satchmo.Relation.Data
import Satchmo.Relation.Op
import qualified Satchmo.Counting as C

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

complete r = empty $ complement r

disjoint r s = do
    i <- intersection r s
    empty i

equals r s = do
    rs <- implies r s
    sr <- implies s r
    and [ rs, sr ]

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

regular, regular_in_degree, regular_out_degree, max_in_degree, min_in_degree, max_out_degree, min_out_degree
  :: ( Ix a, Ix b, MonadSAT m) => Int -> Relation a b -> m Boolean

regular deg r = monadic and [ regular_in_degree deg r, regular_out_degree deg r ]

regular_out_degree = out_degree_helper exactly
max_out_degree = out_degree_helper atmost
min_out_degree = out_degree_helper atleast
regular_in_degree deg r = regular_out_degree deg $ mirror r
max_in_degree deg r = max_out_degree deg $ mirror r
min_in_degree deg r = min_out_degree deg $ mirror r


out_degree_helper f deg r = monadic and $ do
    let ((a,b),(c,d)) = bounds r
    x <- range ( a , c )
    return $ f deg $ do 
        y <- range (b,d)
        return $ r ! (x,y)

transitive :: ( Ix a, MonadSAT m ) 
           => Relation a a -> m Boolean
{-# specialize inline transitive :: ( Ix a ) => Relation a a -> SAT Boolean #-}      
transitive r = do
    r2 <- product r r
    implies r2 r

-- | relation R is a function iff for each x,
-- there is exactly one y such that R(x,y)
is_function :: (Ix a, Ix b, MonadSAT m)
         => Relation a b -> m Boolean
is_function r = regular_out_degree 1 r

-- | relation R is a partial function iff for each x,
-- there is at most one y such that R(x,y)
is_partial_function :: (Ix a, Ix b, MonadSAT m)
         => Relation a b -> m Boolean
is_partial_function r = max_out_degree 1 r


is_bijection :: (Ix a, Ix b, MonadSAT m)
         => Relation a b -> m Boolean
is_bijection r = monadic and [ is_function r , is_function (mirror r) ]

is_permutation :: (Ix a, MonadSAT m)
                  => Relation a a -> m Boolean
is_permutation r = is_bijection r
