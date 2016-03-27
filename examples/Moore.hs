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
import Control.Monad ( void, when, forM )

main :: IO ( )
main = do
  argv <- getArgs
  case argv of
    [ d, k, n ] -> void $ mainf ( read d ) (read k) (read n) Nothing
    [ d, k, n, s ] -> void $ mainf ( read d ) (read k) (read n) (Just $ read s)
    [ d, k ] -> do
      let go d k n ms = do
            ok <- mainf d k n ms
            when ok $ go d k (n+1) ms
      go (read d) (read k) 1 Nothing
    [] -> void $ mainf 3 2 10 Nothing -- petersen

mainf d k n ms = do
  putStrLn $ unwords [ "degree <=", show d, "diameter <=", show k, "nodes ==", show n, "sym", show ms ]
  mg <- solve $ moore d k n ms
  case mg of
    Just g -> do printA g ; return True
    Nothing -> return False

moore :: Int -> Int -> Int -> Maybe Int
      -> SAT (SAT (A.Array (Int,Int) Bool))
moore d k n ms = do
  g <- R.symmetric_relation ((0,0),(n-1,n-1))
  case ms of
    Nothing -> return ()
    Just s -> do
      let f x = mod (x + s) n ; f2 (x,y) = (f x, f y)
      void $ forM (R.indices g) $ \ i -> do
        ok <- B.equals2 (g R.! i) (g R.! f2 i)
        B.assert [ok]
  B.monadic B.assert [ R.reflexive g ]
  B.monadic B.assert [ R.max_in_degree (d+1) g ]
  B.monadic B.assert [ R.max_out_degree (d+1) g ]
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

