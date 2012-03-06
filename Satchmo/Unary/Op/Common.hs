{-# language NoMonomorphismRestriction #-}
{-# language PatternSignatures #-}

module Satchmo.Unary.Op.Common 
       
( iszero, equals
, lt, le, ge, eq, gt
, min, max
, minimum, maximum
, select
, add_quadratic
)          
       
where


import Prelude 
  hiding ( and, or, not, compare, min, max, minimum, maximum )
import qualified Prelude

import qualified Satchmo.Code as C

import Satchmo.Unary.Data 
    (Number, make, bits, width, constant)

import Satchmo.Boolean (MonadSAT, Boolean, Booleans, fun2, fun3, and, or, not, xor, assert, boolean, monadic)
import qualified  Satchmo.Boolean as B

import Control.Monad ( forM, when, foldM, guard )
import qualified Data.Map as M
import Data.List ( transpose )

iszero n = case bits n of
    [] -> B.constant True
    x : xs -> return $ not x
    
extended :: MonadSAT m 
         => ( [(Boolean,Boolean)] -> m a )
         -> Number -> Number
         -> m a
extended action a b = do
    f <- B.constant False
    let zipf [] [] = []
        zipf (x:xs) [] = (x,f) : zipf xs []
        zipf [] (y:ys) = (f,y) : zipf [] ys
        zipf (x:xs) (y:ys) = (x,y) : zipf xs ys
    action $ zipf ( bits a ) ( bits b )    
        

le, ge, eq, equals, gt, lt 
  :: MonadSAT m => Number -> Number -> m Boolean

for = flip map

equals = extended $ \ xys -> monadic and $ 
    for xys $ \ (x,y) -> fun2 (==) x y

le = extended $ \ xys -> monadic and $ 
    for xys $ \ (x,y) -> fun2 (<=) x y

ge = flip le

eq = equals

lt a b = fmap not $ ge a b

gt = flip lt

min a b = do 
    cs <- extended ( \ xys -> 
        forM xys $ \ (x,y) -> and [x,y] ) a b
    return $ make cs                              
                          
max a b = do
    cs <- extended ( \ xys -> 
        forM xys $ \ (x,y) -> or [x,y] ) a b
    return $ make cs                      

-- | maximum (x:xs) = foldM max x xs
maximum [x] = return x
maximum xs | Prelude.not ( null xs ) = do
    f <- B.constant False
    let w = Prelude.maximum $ map width xs
        fill x = bits x ++ replicate (w - width x) f
    ys <- forM ( transpose $ map fill xs ) B.or
    return $ make ys

-- | minimum (x:xs) = foldM min x xs
minimum [x] = return x
minimum xs | Prelude.not ( null xs ) = do
    f <- B.constant False
    let w = Prelude.maximum $ map width xs
        fill x = bits x ++ replicate (w - width x) f
    ys <- forM ( transpose $ map fill xs ) B.and
    return $ make ys



select f a = do
    bs <- forM ( bits a ) $ \ b -> and [f,b]
    return $ make bs


add_quadratic :: MonadSAT m => Int -> Number -> Number -> m Number
add_quadratic width a b = do
    t <- B.constant True
    pairs <- sequence $ do
        (i,x) <- zip [0 .. ] $ t : bits a
        (j,y) <- zip [0 .. ] $ t : bits b
        guard $ i+j > 0
        guard $ i+j <= width + 1
        return $ do z <- and [x,y] ; return (i+j, [z])
    cs <- forM ( map snd $ M.toAscList $ M.fromListWith (++) pairs ) or
    let ( pre, post ) = splitAt width cs
    case post of
        [] -> return ()
        carry : _ -> assert [ not carry ]        
    return $ make pre
    