-- | find incidence matrix of projective plane of given order
-- example usage: ./dist/build/PP/PP 2

{-# language PatternSignatures #-}
{-# language FlexibleContexts #-}

import Prelude hiding ( not, and, or )
import qualified Prelude

import Satchmo.Relation
import Satchmo.Code
import Satchmo.Boolean

import qualified Satchmo.Counting.Binary as CB
import qualified Satchmo.Counting.Unary  as CU
import qualified Satchmo.Counting.Direct as CD

import qualified Satchmo.Binary as B

import Satchmo.SAT.Mini

import Data.List (sort)
import qualified Data.Array as A
import Control.Monad ( guard, when, forM, void )
import System.Environment
import Data.Ix ( range)


main :: IO ()
main = do
    argv <- getArgs
    let [ o ] = case argv of
                     [] -> [5]
                     _  -> map read argv
    Just ( a :: A.Array (Int,Int) Bool ) <- solve $ pp o
    putStrLn $ unlines $ do
         let ((u,l),(o,r)) = A.bounds a
         x <- [ u .. o ]
         return $ unwords $ do 
             y <- [ l .. r ]
             return $ if a A.! (x,y) then "* " else ". "

fill k cs = replicate (k - length cs) ' ' ++ cs

pp o = do
    let n = o*o + o + 1
        points = [ 1 .. n ] ; lines = [ 1 .. n ]
    i :: Relation Int Int <- relation ((1,1),(n,n))
    contains (o+1) i 
    contains (o+1) $ mirror i 
    any_two_determine_one i
    any_two_determine_one $ mirror i
    monotone i
    monotone $ transpose i
    return $ decode i

transpose a =
  let ((1,1),(h,w)) = bounds a
  in  build ((1,1),(w,h)) $ do
         ((x,y),v) <- assocs a
         return ((y,x),v)

-- | see  http://www.maa.org/programs/maa-awards/writing-awards/the-search-for-a-finite-projective-plane-of-order-10
fixed_start o i = do
  let n =  o*o + o + 1
  return ()
    
monotone i = assertM $ do
    let ((1,1),(points, lines)) = bounds i    
        rows = for [ 1 .. points ] $ \ p -> 
            B.make $ for [ 1 .. lines ] $ \ l -> i ! (p,l)
    monadic and $ for ( zip rows $ tail rows ) $ \ (r, r') -> 
        B.lt r r'

contains o i = assertM $ do 
    let ((1,1),(points, lines)) = bounds i
    monadic and $ for [ 1 .. points ] $ \ p -> 
        monadic ( CB.exactly o ) $ for [ 1 .. lines ] $ \ l -> 
          return $ i ! (p, l)
                
any_two_determine_one i = assertM $ do
    let ((1,1),(points, lines)) = bounds i
    monadic and $ for [1..points] $ \ p ->                              
        monadic and $ for [p+1 .. points] $ \ q -> 
            monadic ( CB.exactly 1 ) $ for [1 .. lines] $ \ l -> 
                and [ i ! (p,l),  i ! (q,l) ]
        
for = flip map

assertM this = do x <- this ; assert [x]
