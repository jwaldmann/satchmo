module Satchmo.Counting 

( atleast
, atmost
, exactly
)

where

import Prelude hiding ( and, or, not )

import Satchmo.Boolean

atleast_block :: MonadSAT m => Int -> [ Boolean ] -> m [ Boolean ]
atleast_block k [] = do
    t <- constant True
    f <- constant False
    return $ t : replicate k f
atleast_block k (x:xs) = do
    cs <- atleast_block k xs
    sequence $ do
        i <- [ 0 .. k ]
        return $ if i == 0 then return $ cs !! 0
                 else do
                     p <- and [ x, cs !! (i-1) ]
                     or [ cs !! i, p ]

atleast :: MonadSAT m => Int -> [ Boolean ] -> m Boolean
atleast k xs = do
    cs <- atleast_block k xs
    return $ cs !! k
        

atmost_block :: MonadSAT m => Int -> [ Boolean ] -> m [ Boolean ]
atmost_block k [] = do
    t <- constant $ True
    return $ replicate (k+1) t
atmost_block k (x:xs) = do
    cs <- atmost_block k xs
    sequence $ do
        i <- [ 0 .. k ]
        return $ do
            f <- constant False
            p <- and [ x, if i > 0 then cs !! (i-1) else f ]
            q <- and [ not x, cs !! i ]
            or [ p, q ]

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
    cs <- exactly_block k xs
    sequence $ do
        i <- [ 0 .. k ]
        return $ do
            f <- constant False
            p <- and [ x, if i > 0 then cs !! (i-1) else f ]
            q <- and [ not x, cs !! i ]
            or [ p, q ]

exactly :: MonadSAT m => Int -> [ Boolean ] -> m Boolean
exactly k xs = do
    cs <- exactly_block k xs
    return $ cs !! k
        
