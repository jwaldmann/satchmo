-- | functions in this module have no extra variables but exponential cost.

module Satchmo.Counting.Direct 

( atleast
, atmost
, exactly
)

where

import Satchmo.Boolean ( Boolean, MonadSAT )  
import qualified Satchmo.Boolean as B

import Control.Monad ( forM )

select :: Int -> [a] -> [[a]]
select 0 xs = [[]]
select k [] = []
select k (x:xs) =
  select k xs ++ (map (x:) $ select (k-1) xs)

atleast :: MonadSAT m => Int -> [ Boolean ] -> m Boolean
atleast k xs = B.or =<< forM (select k xs) B.and

atmost :: MonadSAT m => Int -> [ Boolean ] -> m Boolean
atmost k xs = atleast (length xs - k) $ map B.not xs

exactly :: MonadSAT m => Int -> [ Boolean ] -> m Boolean
exactly k xs = do
  this <- atleast k xs
  that <- atmost k xs
  this B.&& that
  
        
