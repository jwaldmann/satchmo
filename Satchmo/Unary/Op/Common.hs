{-# language NoMonomorphismRestriction #-}
{-# language PatternSignatures #-}

module Satchmo.Unary.Op.Common 
       
( iszero, equals
, lt, le, ge, eq, gt
, min, max
, minimum, maximum
, select, antiselect
, add_quadratic, add_via_merge
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


-- | when f is False, switch off all bits
select f a = do
    bs <- forM ( bits a ) $ \ b -> and [f,b]
    return $ make bs

-- | when p is True, switch ON all bits
antiselect p n = do
    bs <- forM ( bits n ) $ \ b -> B.or [p, b]
    return $ make bs

cutoff :: MonadSAT m => Maybe Int -> [Boolean] -> m Number
cutoff mwidth bs = case mwidth of
        Nothing -> return $ make bs
        Just width -> do
            let ( pre, post ) = splitAt width bs
            case post of
                [] -> return ()
                carry : _ -> assert [ not carry ]        
            return $ make pre

-- | for both "add" methods: if first arg is Nothing, 
-- then result length is sum of argument lengths (cannot overflow).
-- else result is cut off (overflow => unsatisfiable)
add_quadratic :: MonadSAT m => Maybe Int -> Number -> Number -> m Number
add_quadratic mwidth a b = do
    t <- B.constant True
    pairs <- sequence $ do
        (i,x) <- zip [0 .. ] $ t : bits a
        (j,y) <- zip [0 .. ] $ t : bits b
        guard $ i+j > 0
        guard $ case mwidth of
            Just width -> i+j <= width + 1
            Nothing    -> True
        return $ do z <- and [x,y] ; return (i+j, [z])
    cs <- forM ( map snd $ M.toAscList $ M.fromListWith (++) pairs ) or
    cutoff mwidth cs


add_via_merge :: MonadSAT m 
                  => Maybe Int 
                  -> Number -> Number 
                  -> m Number
add_via_merge = 
  -- add_via_bitonic_sort
  add_via_odd_even_merge
  
-- | works for all widths
add_via_odd_even_merge mwidth a b = do
    zs <- oe_merge (bits a) (bits b)
    cutoff mwidth zs
    
-- | will fill up the input 
-- such that length is a power of two    
add_via_bitonic_sort mwidth a b = do
    let n = length ( bits a) + length (bits b)
    f <- B.constant False        
    let input =    (bits a) -- decreasing
                ++ replicate (fill n) f
                ++ (reverse $ bits b) -- increasing
    zs <- bitonic_sort input
    cutoff mwidth zs

-- | distance to next power of two
fill n = if n <= 1 then 0 else
            let (d,m) = divMod n 2
            in  m + 2*fill (d+m) 

-- | <http://www.iti.fh-flensburg.de/lang/algorithmen/sortieren/bitonic/bitonicen.htm>
bitonic_sort [ ] = return [ ]    
bitonic_sort [z] = return [z]
bitonic_sort zs = do 
    let (h,0) = divMod (length zs) 2
        (pre, post) = splitAt h zs
    hi <- forM ( zip pre post ) $ \ (x,y) -> or  [x,y]
    lo <- forM ( zip pre post ) $ \ (x,y) -> and [x,y]
    shi <- bitonic_sort hi
    slo <- bitonic_sort lo
    return $ shi ++ slo
    
-- | <http://www.iti.fh-flensburg.de/lang/algorithmen/sortieren/networks/oemen.htm>

oe_merge  [] ys = return ys
oe_merge  xs [] = return xs
oe_merge  [x] [y] = do
    comparator x y
oe_merge  xs ys = do
    let ( xo, xe ) = divide xs
        ( yo, ye ) = divide ys
    m : mo <- oe_merge  xo yo
    me <- oe_merge  xe ye
    re <- repair me mo
    return $ m : re

divide (x : xs) = 
    let ( this, that ) = divide xs
    in  ( x : that, this )
divide [] = ( [], [] )

repair (x:xs) (y:ys) = do
    here <- comparator x y
    later <- repair xs ys
    return $ here ++ later
repair [] [] = return []
repair [x] [] = return [x]
repair [] [y] = return [y]

comparator x y = do
    hi <- Satchmo.Boolean.or [x, y]
    lo <- Satchmo.Boolean.and [x, y]
    return [ hi, lo ]
