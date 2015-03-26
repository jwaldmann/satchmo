-- | functions in this module have no extra variables but exponential cost.

module Satchmo.Counting.Direct 

( atleast
, atmost
, exactly
, assert_implies_atmost
, assert_implies_exactly
)

where

import Satchmo.Boolean ( Boolean, MonadSAT )  
import qualified Satchmo.Boolean as B

import Control.Monad ( forM, forM_ )

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

-- | (and ys) implies (atmost k xs)
assert_implies_atmost ys k xs | k >= 0 = 
  forM_ (select (k+1) xs) $ \ sub -> do
    B.assert $ map B.not ys ++ map B.not sub
assert_implies_atmost ys k _ =
  B.assert $ map B.not ys

assert_implies_atleast ys k xs =
  assert_implies_atmost ys (length xs - k) (map B.not xs)

-- | asserting that  (and ys)  implies  (exactly k xs)
assert_implies_exactly ys k xs = do
  assert_implies_atmost ys k xs
  assert_implies_atleast ys k xs

-- | (atmost k xs) implies (or ys)
assert_atmost_implies xs k ys =
  assert_implies_atleast (map B.not ys) (k+1) xs

assert_atleast_implies xs k ys =
  assert_implies_atmost (map B.not ys) (k+1) xs

  
