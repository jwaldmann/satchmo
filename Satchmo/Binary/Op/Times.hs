module Satchmo.Binary.Op.Times

( times, dot_product
, Overflow (..), times'
)

where

import Prelude hiding ( and, or, not )

import Satchmo.Boolean
import qualified Satchmo.Code as C
import Satchmo.Binary.Data
import Satchmo.Binary.Op.Common

import qualified Data.Map as M
import Control.Monad ( forM )
import Control.Applicative

dot_product :: (MonadSAT m) 
             => ( Maybe Int) 
            -> [ Number ] -> [ Number ] -> m Number
dot_product bound xs ys = do
    cs <- forM ( zip xs ys ) $ \ (x,y) -> product_components Refuse bound (bits x) (bits y)
    make <$> export Refuse bound ( concat cs )

data Overflow = Ignore | Refuse

times :: (MonadSAT m) 
             => Maybe Int
             -> Number -> Number -> m Number
times bound a b =
  make <$> times' Refuse bound (bits a) (bits b)

times' over bound a b = do
    kzs <- product_components over bound a b
    export over bound kzs

product_components over bound a b = sequence $ do
    ( i , x ) <- zip [ 0 .. ] a
    ( j , y ) <- zip [ 0 .. ] b        
    return $ do
        z <- and [ x, y ]
        if ( case bound of Nothing -> False ; Just b -> i+j >= b )
             then do
                case over of
                  Ignore -> return ()
                  Refuse -> assert [ not z ]
                return ( i+j , [ ] )
             else do
                return ( i+j , [z] ) 

export over bound kzs = do 
    m <- reduce over bound $ M.fromListWith (++) kzs
    case M.maxViewWithKey m of
        Nothing -> return []
        Just ((k,_) , _) -> do 
              return $ do 
                    i <- [ 0 .. k ] 
                    let { [ b ] = m M.! i }  
                    return b

reduce over bound m = case M.minViewWithKey m of
    Nothing -> return M.empty
    Just ((k, bs), rest ) -> 
        if ( case bound of Nothing -> False ; Just b -> k >= b )
        then do
            forM bs $ \ b -> case over of
              Refuse -> assert [ not b ]
              Ignore -> return ()
            reduce over bound rest
        else case bs of
            [] -> reduce over bound rest
            [x] -> do
                m' <- reduce over bound rest
                return $ M.unionWith (error "huh") m' 
                       $ M.fromList [(k,[x])] 
            [x,y] -> do
                (r,c) <- half_adder x y
                reduce over bound $ M.unionWith (++) rest
                       $ M.fromList [ (k,[r]), (k+1, [c]) ] 
            (x:y:z:more) -> do
                (r,c) <- full_adder x y z
                reduce over bound $ M.unionWith (++) rest
                       $ M.fromList [ (k, more ++ [r]), (k+1, [c]) ] 


