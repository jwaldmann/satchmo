-- | compute Knight's tour on a rectangular chess board.
-- example usage: ./dist/build/HC/HC 8 8
-- should find and print a solution in < 10 seconds.

{-# language PatternSignatures #-}

import Prelude hiding ( not )
import qualified Prelude

import Satchmo.Relation
import Satchmo.Code
import Satchmo.Boolean
import Satchmo.Counting

import Satchmo.SAT.Mini

import Data.List (sort)
import qualified Data.Array as A
import Control.Monad ( guard, when, forM )
import System.Environment
import Data.Ix ( range)

-- | command line arguments: m n
-- compute knight's tour on  m x n  chess board

main :: IO ()
main = do
    argv <- getArgs
    let [ m, n ] = map read argv
    Just a <- solve $ tour m n
    putStrLn $ unlines $ do
         let ((u,l),(o,r)) = A.bounds a
         x <- [u .. o]
         return $ unwords $ do 
             y <- [ l ..r ]
             return $ fill 4 $ show $ a A.! (x,y)

fill k cs = replicate (k - length cs) ' ' ++ cs

tour m n = do
    let s = m * n
        felder = range ((1,1),(m,n))
    p :: Relation Int (Int,Int) <- bijection ((1,(1,1)), (s,(m,n)))
    forM ( zip [1..s] $ rotate 1 [1..s] ) $ \ (i,i') -> do
        forM felder $ \ j -> 
            assert $ not ( p!(i,j)) : do
                k <- felder
                guard $ reaches j k
                return $ p ! (i',k) 
        forM felder $ \ k -> 
            assert $ not ( p!(i',k)) : do
                j <- felder
                guard $ reaches j k
                return $ p ! (i,j) 
{-
        forM felder $ \ j -> 
            forM felder $ \ k -> do
                c <- constant $ reaches j k
                assert [ not $ p!(i,j), not $ p!(i',k)
                       , c
                       ]
-}
    return $ do
        a <- decode p
        return $ A.array ((1,1),(m,n)) $ do
            ((i :: Int , p::(Int,Int)),True) <- A.assocs a
            return (p,i)

bijection :: (A.Ix a, A.Ix b, MonadSAT m) 
                   => ((a,b),(a,b)) 
                   -> m ( Relation a b )
bijection bnd = do
    let ((u,l),(o,r)) = bnd
    a <- relation bnd

    -- official encoding: exactly one per row, exactly one per column
    sequence_ $ do
        x <- A.range (u,o)
        return $ monadic assert $ return $ exactly 1 $ do y <- A.range (l,r) ; return $ a!(x,y)
    sequence_ $ do
        y <- A.range (l,r)
        return $ monadic assert $ return $ exactly 1 $ do x <- A.range (u,o) ; return $ a!(x,y)

{-
    -- this should be enough: at least one per row, at most one per column
    sequence_ $ do
        x <- A.range (u,o)
        return $ assert $ do y <- A.range (l,r) ; return $ a!(x,y)
    sequence_ $ do
        y <- A.range (l,r)
        return $ monadic assert $ return $ atmost 1 $ do x <- A.range (u,o) ; return $ a!(x,y)
-}
    return a                                                   


reaches (px,py) (qx,qy) = 
    5 == (px - qx)^2 + (py - qy)^2

rotate :: Int -> [a] -> [a]
rotate k xs = 
    let ( pre, post ) = splitAt k xs
    in  post ++ pre
    
