module Satchmo.Binary.Op.Times where

import Prelude hiding ( and, or, not )

import Satchmo.Boolean
import qualified Satchmo.Code as C
import Satchmo.Binary.Data
import Satchmo.Binary.Op.Common

import qualified Data.Map as M
import Control.Monad ( forM )


dot_product :: (MonadSAT m) 
             => ( Maybe Int) 
            -> [ Number ] -> [ Number ] -> m Number
dot_product bound xs ys = do
    cs <- forM ( zip xs ys ) $ \ (x,y) -> product_components bound x y
    export bound $ concat cs

times :: (MonadSAT m) 
             => Maybe Int
             -> Number -> Number -> m Number
times bound a b = do
    kzs <- product_components bound a b
    export bound kzs

product_components :: MonadSAT m
    => Maybe Int
    -> Number -> Number -> m [ (Int, [Boolean]) ]
product_components bound a b = sequence $ do
    ( i , x ) <- zip [ 0 .. ] $ bits a
    ( j , y ) <- zip [ 0 .. ] $ bits b        
    return $ do
        z <- and [ x, y ]
        if ( case bound of Nothing -> False ; Just b -> i+j >= b )
             then do assert [ not z ] ; return ( i+j , [ ] )
             else do                    return ( i+j , [z] ) 

export :: MonadSAT m => Maybe Int -> [(Int,[Boolean])] -> m Number
export bound kzs = do 
    m <- reduce bound $ M.fromListWith (++) kzs
    case M.maxViewWithKey m of
        Nothing -> return $ make []
        Just ((k,_) , _) -> do 
              return $ make $ do 
                    i <- [ 0 .. k ] 
                    let { [ b ] = m M.! i }  
                    return b

reduce bound m = case M.minViewWithKey m of
    Nothing -> return M.empty
    Just ((k, bs), rest ) -> 
        if ( case bound of Nothing -> False ; Just b -> k >= b )
        then do
            forM bs $ \ b -> assert [ not b ]
            reduce bound rest
        else case bs of
            [] -> reduce bound rest
            [x] -> do
                m' <- reduce bound rest
                return $ M.unionWith (error "huh") m' 
                       $ M.fromList [(k,[x])] 
            [x,y] -> do
                (r,c) <- half_adder x y
                reduce bound $ M.unionWith (++) rest
                       $ M.fromList [ (k,[r]), (k+1, [c]) ] 
            (x:y:z:more) -> do
                (r,c) <- full_adder x y z
                reduce bound $ M.unionWith (++) rest
                       $ M.fromList [ (k, more ++ [r]), (k+1, [c]) ] 


