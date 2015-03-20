-- | find colouring without complete subgraphs
-- example usage: ./dist/build/Ramsey/Ramsey 3 3 3 16
-- last number is size of graph,
-- earlier numbers are sizes of forbidden cliques

{-# language PatternSignatures #-}

import Prelude hiding ( not, and, or, product )
import qualified Prelude

import Satchmo.Relation
import Satchmo.Code
import Satchmo.Boolean hiding ( implies )
import Satchmo.Counting

import qualified Satchmo.Binary as B

import Satchmo.SAT.CNF

import Data.List (sort, tails)
import qualified Data.Array as A
import Control.Monad ( guard, when, forM, foldM, void )
import System.Environment
import Data.Ix ( range)


main :: IO ()
main = do
    argv <- getArgs
    let ns = map read $ case argv of
          [] -> [ "3", "3", "5" ] -- small numbers, else it will blow up
          _ -> argv
        cs = init ns 
        n = last ns
    Just ( p : fs ) <- solve $ ramsey cs n
    forM ( zip [ 1.. ] fs ) $ \ (k, f) -> do 
        putStrLn $ unwords [ "colour", show k ]
        printA f
    putStrLn "with isomorphism" ; printA p

printA :: A.Array (Int,Int) Bool -> IO ()
printA a = putStrLn $ unlines $ do
         let ((u,l),(o,r)) = A.bounds a
         x <- [u .. o]
         return $ unwords $ do 
             y <- [ l ..r ]
             return $ case a A.! (x,y) of
                  True -> "* " ; False -> ". "

ramsey (cs :: [Int]) (n :: Int) = do
    fs <- forM cs $ \ c -> 
         relation ((1 :: Int,1 :: Int),(n,n))
    
    p <- relation ((1,1),(n,n))
    -- forM fs $ isomorphism p

    -- forM fs $ cyclic 3

    when False $ void $ do
        forM [ 1 .. n ] $ \ x -> 
            forM [ x + 1 .. n ] $ \ y -> 
                assertM $ exactly 1 $ 
                    for fs $ \ f -> f ! (x,y) 
    when True $ void $ do
        forM [ 1 .. n ] $ \ x -> 
            forM [ x + 1 .. n ] $ \ y -> 
                assert $ for fs $ \ f -> f ! (x,y)

    forM ( zip cs fs ) $ \ (c,f) -> 
        forM ( cliquesA c [1..n] ) $ \ xs ->
            assert $ for ( cliquesA 2 xs ) $ \ [x,y] -> not $ f ! (x,y)
    return $ forM (p : fs) decode
    
isomorphism p e = do
    assertM $ regular 1 p
    assertM $ regular 1 $ mirror p
    e' <- foldM product ( mirror p ) [ e, p ]
    assertM $ implies e e'
    assertM $ implies e' e

cyclic off f = forM ( indices f ) $ \ (i,j) -> 
    when ( off < i Prelude.&& i < j ) 
         $ assert_fun2 (==) ( f!(i,j) ) (f!(i-off,j-off))

cliquesA k xs = 
      let -- spec:  c!(i,j) == cliques i (drop j xs)
          bnd = ((0,0),(k, length xs))
          c = A.array bnd $ do
            (i,j) <- A.range bnd
            return ( (i,j)
                   , if i == 0 then [ [] ]
                     else if i > length xs - j then []               
                     else c A.! (i,j+1) 
                          ++ map (xs !! j : ) ( c A.! (i-1,j+1))
                   )             
      in  c A.! (k,0)         

cliques 0 xs = return []
cliques k xs | k > length xs = []
cliques k (x:xs) =
    cliques k xs ++ map (x :) ( cliques (k-1) xs )

for = flip map

assertM this = do x <- this ; assert [x]
