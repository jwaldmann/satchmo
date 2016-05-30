-- | Find 2-colouring of [1 .. n ]
-- without Pythagorean triples.
-- This problem got recent attention via
-- http://arxiv.org/abs/1605.00723 .
-- Our encoding here is straightforward.

{-# language FlexibleContexts #-}

import qualified Satchmo.Boolean as B
import Satchmo.Code (decode)	 
import Satchmo.SAT.Mini

import Control.Monad ( guard, forM_, replicateM )
import System.Environment

main = do
  argv <- getArgs
  run $ case argv of
      [] -> 5000
      [s] -> read s

run :: Int -> IO ()
run n = do
  Just xs <- solve $ pyth n
  print $ map fromEnum (xs  :: [Bool])

pyth n = do
  xs <- replicateM n B.boolean
  forM_ (triples n) $ \ (a,b,c) -> do
    let bits = map (xs!!) $ map pred [a,b,c]
    B.assert $ map    id bits
    B.assert $ map B.not bits
  return $ decode xs

triples n = do
  c <- [1 .. n]
  solves 3 (c-1) c

-- | produce triples (a,b,c) of positive numbers
-- with  a < b  and  a^2 + b^2 == c^2.
-- increase a, decrease b, keep c.
-- inefficiencies: we could avoid all ^2.
solves :: Int -> Int -> Int -> [(Int,Int,Int)]
solves a b c =
  if a >= b then []
  else case compare (a^2 + b^2) (c^2) of
    LT ->           solves (a+1)   b   c
    EQ -> (a,b,c) : solves (a+1) (b-1) c
    GT ->           solves   a   (b-1) c
    