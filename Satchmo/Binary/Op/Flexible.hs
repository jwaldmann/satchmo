{-# language MultiParamTypeClasses, PatternGuards #-}

-- | operations from this module cannot overflow.
-- instead they increase the bit width.

module Satchmo.Binary.Op.Flexible

( add, times
, add_with_carry, times1, shift
, module Satchmo.Binary.Data
, module Satchmo.Binary.Op.Common
)

where

import Prelude hiding ( and, or, not )

import Satchmo.Boolean
import qualified Satchmo.Code as C
import Satchmo.Binary.Data
import Satchmo.Binary.Op.Common
import Satchmo.Counting

import qualified Data.Map as M

add :: (MonadSAT m) => Number -> Number -> m Number
add a b = do
    false <- Satchmo.Boolean.constant False
    ( zs, carry ) <- 
        add_with_carry false (bits a) (bits b)
    return $ make $ zs ++ [carry]

add_with_carry :: (MonadSAT m) => Boolean 
               -> Booleans -> Booleans
               -> m ( Booleans, Boolean )
add_with_carry cin [] [] = return ( [], cin )
add_with_carry cin (x:xs) [] = do
    (z, c) <- half_adder cin x
    ( zs, cout ) <- add_with_carry c xs []
    return ( z : zs, cout )
add_with_carry cin [] (y:ys) = do
    add_with_carry cin (y:ys) []
add_with_carry cin (x:xs ) (y:ys) = do
    (z, c) <- full_adder cin x y
    ( zs, cout ) <- add_with_carry c xs ys
    return ( z : zs, cout )

times :: (MonadSAT m) => Number -> Number -> m Number
times = -- plain_times 
      better_times

plain_times :: (MonadSAT m) => Number -> Number -> m Number
plain_times a b | [] <- bits a = return a
plain_times a b | [] <- bits b = return b
plain_times a b | [x] <- bits a = times1 x b
plain_times a b | [y] <- bits b = times1 y a
plain_times a b | x:xs <- bits a = do
    xys  <- times1 x b
    xsys <- plain_times (make xs) b
    zs <- shift xsys
    add xys zs

-- | multiply by 2
shift :: (MonadSAT m) => Number -> m Number
shift a = do
    false <- Satchmo.Boolean.constant False 
    return $ make $ false : bits a

times1 :: (MonadSAT m) => Boolean -> Number -> m Number
times1 x b = do
    zs <- mapM ( \ y -> and [x,y] ) $ bits b
    return $ make zs


better_times :: (MonadSAT m) 
             => Number -> Number -> m Number
better_times a b = do
    kzs <- sequence $ do
          ( i , x ) <- zip [ 0 .. ] $ bits a
          ( j , y ) <- zip [ 0 .. ] $ bits b
          return $ do
                  z <- and [ x, y ]
                  return ( i+j , [z] ) 
    m <- reduce $ M.fromListWith (++) kzs
    let Just ((k,_) , _) = M.maxViewWithKey m
    return $ make $ do i <- [ 0 .. k ] ; let { [ b ] = m M.! i } ; return b

reduce m = case M.minViewWithKey m of
    Nothing -> return M.empty
    Just ((k, bs), rest ) -> case bs of
        [x] -> do
            m' <- reduce rest
            return $ M.unionWith (error "huh") m' 
                   $ M.fromList [(k,[x])] 
        [x,y] -> do
            (r,c) <- half_adder x y
            reduce $ M.unionWith (++) rest
                   $ M.fromList [ (k,[r]), (k+1, [c]) ] 
        (x:y:z:more) -> do
            (r,c) <- full_adder x y z
            reduce $ M.unionWith (++) rest
                   $ M.fromList [ (k, more ++ [r]), (k+1, [c]) ] 

