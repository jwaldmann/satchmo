-- | compute a (r,g)-Cage of order n
-- example usage: ./dist/build/Cage/Cage 3 5 10
-- should find the Petersen graph

{-# language PatternSignatures #-}

import Prelude hiding ( not, product )
import qualified Prelude

import Satchmo.Relation
import Satchmo.Code
import Satchmo.Boolean
import Satchmo.Counting

import Satchmo.Solver.Minisat

import Data.List (inits, tails)
import qualified Data.Array as A
import Control.Monad ( guard, when, forM, foldM )
import System.Environment
import Data.Ix ( range)

main :: IO ()
main = do
    argv <- getArgs
    let [ r, g, n ] = map read argv
    Just [e, p ] <- solve $ cage r g n
    printA e
    printA p


printA :: A.Array (Int,Int) Bool -> IO ()
printA a = putStrLn $ unlines $ do
         let ((u,l),(o,r)) = A.bounds a
         x <- [u .. o]
         return $ unwords $ do 
             y <- [ l ..r ]
             return $ case a A.! (x,y) of
                  True -> "* " ; False -> ". "

cage r g n = do
    e :: Relation Int Int <- relation ((1,1), (n,n))
                                   
    p :: Relation Int Int <- relation ((1,1), (n,n))
    isomorphism p e

    assertM $ symmetric e
    assertM $ irreflexive e                                   
    assertM $ regular r e

    forM [ 3 .. g - 1 ] $ \ g' -> nocircle g' e

    return $ decode [e, p]

isomorphism p e = do
    assertM $ regular 1 p
    assertM $ regular 1 $ mirror p
    e' <- foldM product ( mirror p ) [ e, p ]
    assertM $ implies e e'
    assertM $ implies e' e
    
nocircle g e = do
    let ((1,1), (n,_)) = bounds e
    forM ( filter minimal $ subs g [ 1 .. n ] ) $ \ xs -> 
        assert $ for ( zip xs $ tail xs ++ [ head xs ] ) 
               $ \ (x,y) -> not $ e !(x,y)

minimal (x:xs) = all (> x) xs

subs :: Int -> [a] -> [[a]]    
subs 0 xs = return []    
subs k xs = do
    (pre, y : post) <- splits xs
    ys <- subs (k-1) $ pre ++ post
    return $ y : ys
    
splits xs = zip ( inits xs ) ( tails xs )        
assertM action = do x <- action ; assert [x]
for = flip map    

