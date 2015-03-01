module Satchmo.Counting.Binary

( atleast
, atmost
, exactly
, count
)

where

import Prelude hiding ( and, or, not )

import Satchmo.Boolean
import Satchmo.Binary

import Satchmo.SAT ( SAT) -- for specializations

{-# specialize inline atleast :: Int -> [ Boolean] -> SAT Boolean #-}
{-# specialize inline atmost  :: Int -> [ Boolean] -> SAT Boolean #-}
{-# specialize inline exactly :: Int -> [ Boolean] -> SAT Boolean #-}
{-# specialize inline count :: [ Boolean] -> SAT Number #-}

count :: MonadSAT m => [ Boolean ] -> m Number
count bits
  = collect (Satchmo.Binary.constant 0) Satchmo.Binary.add
  $ map ( \ bit -> Satchmo.Binary.make [bit] )
  $ bits

collect :: Monad m => m a -> (a -> a -> m a) -> [a] -> m a
collect z b xs = case xs of
  [] -> z
  [x] -> return x
  (x:y:zs) -> b x y >>= \ c -> collect z b (zs ++ [c])
    
atleast :: MonadSAT m => Int -> [ Boolean ] -> m Boolean
atleast k xs = common ge k xs

atmost :: MonadSAT m => Int -> [ Boolean ] -> m Boolean
atmost k xs = common le k xs
        
exactly :: MonadSAT m => Int -> [ Boolean ] -> m Boolean
exactly k xs = common eq k xs

common :: MonadSAT m
       => (Number -> Number -> m b)
       -> Int -> [ Boolean ] -> m b
common cmp k xs = do
  c <- count xs
  goal <- Satchmo.Binary.constant $ fromIntegral k
  cmp c goal 

