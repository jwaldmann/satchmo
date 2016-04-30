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

data NumCarries =
  NumCarries { num:: Number,carries:: [Boolean]}

zro = NumCarries {num=make [], carries=[] }
mke 0 b = NumCarries {num=make[],carries=[b]}
mke w b | w > 0 = NumCarries {num=make[b],carries=[]}
pls w x y = do
  z <- Satchmo.Binary.add (num x) (num y)
  let (pre,post) = splitAt w $ bits z
  return $ NumCarries
     { num = make pre
     , carries = post ++ carries x ++ carries y
     }

count_and_carry width bits 
  = collect (return zro) (pls width) $ map (mke width) bits
  
collect :: Monad m => m a -> (a -> a -> m a) -> [a] -> m a
collect z b xs = case xs of
  [] -> z
  [x] -> return x
  (x:y:zs) -> b x y >>= \ c -> collect z b (zs ++ [c])

atleast :: MonadSAT m => Int -> [ Boolean ] -> m Boolean
atleast k xs = common True ge k xs

atmost :: MonadSAT m => Int -> [ Boolean ] -> m Boolean
atmost k xs = common False le k xs
        
exactly :: MonadSAT m => Int -> [ Boolean ] -> m Boolean
exactly k xs = common False eq k xs

common :: MonadSAT m
       => Bool 
       -> (Number -> Number -> m Boolean)
       -> Int -> [ Boolean ] -> m Boolean
common may_overflow cmp k xs = do
  let bk = Satchmo.Binary.toBinary $ fromIntegral k
  NumCarries { num=n,carries=cs} <-
    count_and_carry (length bk) xs
  goal <- Satchmo.Binary.constant $ fromIntegral k
  ok <- cmp n goal 
  if may_overflow
    then or $ ok : cs
    else and $ ok : map not cs
         
    


