-- | find van der Warden Colourings
-- (avoiding monochromatic arithmetic sequences)

{-# language ScopedTypeVariables #-}

import Prelude hiding ( not, and, or, product )
import qualified Prelude

import Satchmo.Relation
import Satchmo.Code
import Satchmo.Boolean
import Satchmo.Counting

import Satchmo.SAT.External

import Data.List (sort, tails)
import qualified Data.Array as A
import Control.Monad ( guard, when, forM, foldM )
import System.Environment
import Data.Ix ( range)


main :: IO ()
main = do
    argv <- getArgs
    let [ r, k, n ] = map (read :: String -> Int) argv
    Just f <- solve "glucose" [ "-model"] $ waerden r k n
    printA f

printA :: A.Array (Int,Int) Bool -> IO ()
printA a = putStrLn $ unlines $ do
         let ((u,l),(o,r)) = A.bounds a
         x <- [u .. o]
         return $ unwords $ do 
             y <- [ l ..r ]
             return $ case a A.! (x,y) of
                  True -> "* " ; False -> ". "

waerden :: Int -> Int -> Int
        -> SAT  (Decoder SAT (A.Array (Int,Int) Bool))
waerden r k n = do
    f <- relation ((1 :: Int,1 :: Int),(r,n))
    assertM $ regular 1 $ mirror f
    
    forM ( arithmetics k 1 n ) $ \ xs -> 
        forM [ 1 .. r ] $ \ c -> 
          assert $ for xs $ \ x -> not $ f ! (c,x)
    return $ decode f          

arithmetics k lo hi = do
    step <- [ 1 .. div (hi - lo + 1) (k-1) ]
    start <- [ lo .. hi - (k-1) * step ]
    return $ take k [ start, start + step .. hi ]
    
isomorphism p e = do
    assertM $ regular 1 p
    assertM $ regular 1 $ mirror p
    e' <- foldM product ( mirror p ) [ e, p ]
    assertM $ Satchmo.Relation.implies e e'
    assertM $ Satchmo.Relation.implies e' e

for = flip map

assertM this = do x <- this ; assert [x]
