-- | The all-interval series problem.
-- https://ianm.host.cs.st-andrews.ac.uk/CSPLib/prob/prob007/spec.html
-- As I am reading it, the task is to find one (or all) graceful labellings of  a path.
-- Finding one is easy, you can take [0, n, 1, n-1, 2, .. ]
-- for Definition and Background, see
-- http://www.combinatorics.org/ojs/index.php/eljc/article/view/DS6

{-# language PatternSignatures #-}

import Prelude hiding ( not, product, and, or )
import qualified Prelude

import qualified Satchmo.Relation as R
import Satchmo.Code
import Satchmo.Boolean
import qualified Satchmo.Counting as C

import Satchmo.SAT.Mini

import Data.List (inits, tails, sort)
import qualified Data.Array as A
import Control.Monad ( guard, when, forM, foldM, forM_ )
import System.Environment
import Data.Ix ( range)
import Control.Applicative ((<$>))

main :: IO ()
main = do
    argv <- getArgs
    case argv of
      [ ] -> main_with 5
      [s] -> main_with $ read s

main_with :: Int -> IO ()
main_with n = do
    Just a <- solve $ ais n
    let  xs = do
           let ((u,l),(o,r)) = A.bounds a
           x <- A.range (u,o) 
           let zs = map (\y -> a A.! (x,y) ) (A.range(l,r))
           return $ length $ takeWhile Prelude.not zs
         ds = map abs $ zipWith (-) xs $ drop 1 xs
    print xs
    print $ sort xs == [0 .. n]
    
    print ds
    print $ sort ds == [1 .. n]

ais :: Int
       -> SAT (SAT (A.Array (Int,Int) Bool))
ais n = do
  r :: R.Relation Int Int <-
    R.relation ((0,0),(n,n))
  assertM $ R.is_bijection r
  forM_ [ 1 .. n-1 ] $ \ d -> do
    occs <- concat <$> ( forM [ 0 .. n-1 ] $ \ x -> do
      forM [0 .. n-d] $ \ v -> do 
        up   <- and [ r R.! (x,v), r R.! (x+1,v+d) ]
        down <- and [ r R.! (x,v+d), r R.! (x+1,v) ]
        or [up,down] )
    assertM $ C.exactly 1 occs
  return $ decode r

assertM action = do x <- action ; assert [x]
fromfunc bnd f = R.build bnd $ do i <- A.range bnd ; return (i, f i )
