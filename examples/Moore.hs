-- | graphs n nodes of degree <= d and diameter <= k 
-- see http://combinatoricswiki.org/wiki/The_Degree_Diameter_Problem_for_General_Graphs

-- usage: ./Moore d k n

{-# language FlexibleContexts #-}

import Prelude hiding ( not, or, and )
import qualified Prelude

import qualified Satchmo.Relation as R
import Satchmo.Code
import qualified Satchmo.Boolean as B
import Satchmo.Counting
import Satchmo.SAT.Mini

import qualified Data.Array as A
import System.Environment (getArgs)
import Control.Monad ( void, when )

main :: IO ( )
main = do
  argv <- getArgs
  case argv of
    [ d, k, n ] -> void $ mainf ( read d ) (read k) (read n)
    [ d, k ] -> do
      let go d k n = do
            ok <- mainf d k n
            when ok $ go d k (n+1)
      go (read d) (read k) 1      
    [] -> void $ mainf 3 2 10 -- petersen

mainf d k n = do
  putStrLn $ unwords [ "degree <=", show d, "diameter <=", show k, "nodes ==", show n ]
  mg <- solve $ moore d k n
  case mg of
    Just g -> do printA g ; return True
    Nothing -> return False

moore :: Int -> Int -> Int -> SAT (SAT (A.Array (Int,Int) Bool))
moore d k n = do
  g <- R.symmetric_relation ((1,1),(n,n))
  B.monadic B.assert [ R.reflexive g ]
  B.monadic B.assert [ R.max_degree (d+1) g ]
  p <- R.power k g
  B.monadic B.assert [ R.complete p ]
  return $ decode g

-- | FIXME: this needs to go into a library
printA :: A.Array (Int,Int) Bool -> IO ()
printA a = putStrLn $ unlines $ do
         let ((u,l),(o,r)) = A.bounds a
         x <- [u .. o]
         return $ unwords $ do 
             y <- [ l ..r ]
             return $ case a A.! (x,y) of
                  True -> "* " ; False -> ". "

