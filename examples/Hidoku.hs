-- | Simple Hidoku Benchmark:
-- constraints for an empty board (no hints).
-- argument n: board is  n*n.
-- .
-- The encoding here is in a straightforward style, using "one-hot" encoding
-- for numbers, and  @Relation.Prop.is_bijection@
-- which contains @exactly-one@ constraints that use binary counters.
-- .
-- For discussion of a many more encoding options,
-- see 4.2 and 4.4 of http://nbn-resolving.de/urn:nbn:de:bsz:14-qucosa-158672

{-# language PatternSignatures #-}

import Prelude hiding ( not, product )
import qualified Prelude

import qualified Satchmo.Relation as R
import Satchmo.Code
import Satchmo.Boolean

import Satchmo.SAT.Mini

import Data.List (inits, tails)
import qualified Data.Array as A
import Control.Monad ( guard, when, forM, foldM, forM_ )
import System.Environment
import Data.Ix ( range)

main :: IO ()
main = do
    argv <- getArgs
    case argv of
      [ ] -> main_with 10
      [s] -> main_with $ read s

main_with :: Int -> IO ()
main_with n = do
    Just r <- solve $ hidoku n
    printA n r

printA :: Int -> A.Array ((Int,Int),Int) Bool -> IO ()
printA n a = putStrLn $ unlines $ do
  x <- A.range (1,n)
  return $ unwords $ do 
    y <- A.range (1,n)
    let zs = map (\z -> a A.! ((x,y),z)) (A.range (1,n^2))
        fill n s = replicate (n - length s) ' ' ++ s
    return $ fill 3 $ show $ succ $ length $ takeWhile Prelude.not zs

hidoku :: Int
       -> SAT (SAT (A.Array ((Int,Int),Int) Bool))
hidoku n = do
  r :: R.Relation (Int,Int) Int <-
    R.relation ( ((1,1),1),((n,n),n^2) )
  assertM $ R.is_bijection r
  forM_ (A.range (1 ,n^2 - 1)) $ \ i -> do
    forM_ (A.range ((1,1),(n,n))) $ \ p@(x,y) -> do
      assert $ not (r R.!((x,y),i)) : do
        (dx,dy) <- A.range ((-1,-1),(1,1))
        let q = (x+dx,y+dy)
        guard $ p /= q Prelude.&& A.inRange (R.bounds r) (q,i+1)
        return $ r R.! ((x+dx,y+dy),i+1)
  return $ decode r

assertM action = do x <- action ; assert [x]
