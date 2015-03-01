module Satchmo.Counting 

( atleast
, atmost
, exactly
)

where

import Prelude hiding ( and, or, not )

import Satchmo.Boolean

import Satchmo.SAT ( SAT) -- for specializations

{-# specialize inline atleast :: Int -> [ Boolean] -> SAT Boolean #-}
{-# specialize inline atmost  :: Int -> [ Boolean] -> SAT Boolean #-}
{-# specialize inline exactly :: Int -> [ Boolean] -> SAT Boolean #-}

atleast :: MonadSAT m => Int -> [ Boolean ] -> m Boolean
atleast k xs = fmap not $ atmost (k-1) xs
        

atmost_block :: MonadSAT m => Int -> [ Boolean ] -> m [ Boolean ]
atmost_block k [] = do
    t <- constant $ True
    return $ replicate (k+1) t
atmost_block k (x:xs) = do
    cs <- atmost_block k xs
    f <- constant False
    sequence $ do
        (p,q) <- zip cs ( f : cs )
        return $ do
            fun3  ( \ x p q -> if x then q else p ) x p q

atmost :: MonadSAT m => Int -> [ Boolean ] -> m Boolean
atmost k xs = do
    cs <- atmost_block k xs
    return $ cs !! k
        

exactly_block :: MonadSAT m => Int -> [ Boolean ] -> m [ Boolean ]
exactly_block k [] = do
    t <- constant True
    f <- constant False
    return $ t : replicate k f
exactly_block k (x:xs) = do
    f <- constant False
    cs <- exactly_block k xs
    sequence $ do
        (p,q) <- zip cs ( f : cs )
        return $ do
            fun3 ( \ x p q -> if x then q else p ) x p q

exactly :: MonadSAT m => Int -> [ Boolean ] -> m Boolean
exactly k xs = do
    cs <- exactly_block k xs
    return $ cs !! k
        
