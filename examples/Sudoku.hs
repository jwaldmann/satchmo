-- | Simple Sudoku Benchmark:
-- constraints for an empty board (no hints).
-- argument n: board is (n^2)x(n^2),
-- so standard Sudoku is for n=3

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
      [ ] -> main_with 5
      [s] -> main_with $ read s

main_with :: Int -> IO ()
main_with n = do
    Just r <- solve $ sudoku n
    printA n r

printA :: Int -> A.Array ((Int,Int,Int,Int),(Int,Int)) Bool -> IO ()
printA n a = putStrLn $ unlines $ do
  (x1,x2) <- A.range ((1,1),(n,n))
  return $ unwords $ do 
    (y1,y2) <- A.range ((1,1),(n,n))
    let zs = map (\z -> a A.! ((x1,x2,y1,y2),z) ) (A.range ((1,1),(n,n)) )
        fill n s = replicate (n - length s) ' ' ++ s
    return $ fill 3 $ show $ length $ takeWhile Prelude.not zs

sudoku :: Int
       -> SAT (SAT (A.Array ((Int,Int,Int,Int),(Int,Int)) Bool))
sudoku n = do
  r :: R.Relation (Int,Int,Int,Int) (Int,Int) <-
    R.relation (((1,1,1,1),(1,1)),((n,n,n,n),(n,n)))
  forM_ [ blockA, blockB, blockC ] $ \ bl ->
    forM_ (A.range ((1,1),(n,n))) $ \ (x,y) ->
      assertM $ R.is_bijection $ bl n r (x,y)
  return $ decode r

blockA (n::Int) r (x,y) =
  fromfunc (((1,1),(1,1)),((n,n),(n,n)))
    $ \ ((x1,x2),(y1,y2)) -> r R.! ((x,y,x1,x2),(y1,y2)) 

blockB (n::Int) r (x,y) =
  fromfunc (((1,1),(1,1)),((n,n),(n,n)))
    $ \ ((x1,x2),(y1,y2)) -> r R.! ((x1,x2,x,y),(y1,y2)) 

blockC (n::Int) r (x,y) =
  fromfunc (((1,1),(1,1)),((n,n),(n,n)))
    $ \ ((x1,x2),(y1,y2)) -> r R.! ((x,x1,y,x2),(y1,y2)) 


                             
assertM action = do x <- action ; assert [x]
fromfunc bnd f = R.build bnd $ do i <- A.range bnd ; return (i, f i )
