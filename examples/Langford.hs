-- | The Langford Sequence Problem
-- http://www.csplib.org/Problems/prob024/

{-# language ScopedTypeVariables #-}

import Prelude hiding ( not, product, and, or )
import qualified Prelude

import qualified Satchmo.Relation as R
import Satchmo.Code
import Satchmo.Boolean
import qualified Satchmo.Counting as C

import Satchmo.SAT.Mini

import Data.List (inits, tails)
import qualified Data.Array as A
import Control.Monad ( guard, when, forM, foldM, forM_ )
import System.Environment
import Data.Ix ( range)
import Data.List ( sort )

main :: IO ()
main = do
    argv <- getArgs
    case argv of
      [ ] -> main_with 12
      [s] -> main_with $ read s

main_with :: Int -> IO ()
main_with n = do
    Just a <- solve $ langford n
    let  xs = do
           let ((u,l),(o,r)) = A.bounds a
           y <- A.range (l,r) 
           let zs = map (\x -> a A.! (x,y) ) (A.range(u,o))
           return $ length $ takeWhile Prelude.not zs
    print $ map (\x -> 1 + div x 2) xs

langford :: Int
       -> SAT (SAT (A.Array (Int,Int) Bool))
langford k = do
  --   r(x,y) <=> number (div x 2) is at position y
  r :: R.Relation Int Int <-
    R.relation ((2,1),(2*k+1,2*k))
  assertM $ R.is_bijection r
  false <- constant False
  forM_ [ 1 .. k ] $ \ x -> do
    forM_ [ 1 .. 2*k ] $ \ y -> do
      assert [ not $ r R.! (2*x+0 , y)
             , if x+y+1<=2*k then r R.! (2*x+1, x+y+1) else false
             ]
      assert [ not $ r R.! (2*x+1, y)
             , if y-x-1 >= 1 then r R.! (2*x, y-x-1) else false
             ]
  return $ decode r

assertM action = do x <- action ; assert [x]
fromfunc bnd f = R.build bnd $ do i <- A.range bnd ; return (i, f i )
