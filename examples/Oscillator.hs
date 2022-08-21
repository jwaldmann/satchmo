-- | compute oscillator for Conway's game of life, 
-- cf. http://www.conwaylife.com/wiki/Category:Oscillators
-- example usage: ./dist/build/Life/Life 3 9 9 20
-- arguments are: period, width, height, number of life start cells

{-# language ScopedTypeVariables #-}
{-# language FlexibleContexts #-}

import Prelude hiding ( not, or, and )
import qualified Prelude

import Satchmo.Relation
import Satchmo.Code
import Satchmo.Boolean hiding ( equals, implies )
import Satchmo.Counting.Binary

import Satchmo.SAT.Mini

import Data.List (sort)
import qualified Data.Array as A
import Control.Monad ( guard, when, forM, foldM, void )
import System.Environment
import Data.Ix ( range, inRange )

main :: IO ()
main = void $ do
    argv <- getArgs
    Just gs <- case map read argv of
        []             -> solve $ osc 3 9 9 (Just 20)
        [ p, w       ] -> solve $ osc p w w Nothing
        [ p, w, h    ] -> solve $ osc p w h Nothing
        [ p, w, h, c ] -> solve $ osc p w h $ Just c
    forM ( zip [ 0..  ] gs ) $ \ (t, g) -> do
        putStrLn $ unwords [ "time", show t ]
        printA g

printA :: A.Array (Int,Int) Bool -> IO ()
printA a = putStrLn $ unlines $ do
         let ((u,l),(o,r)) = A.bounds a
         x <- [u .. o]
         return $ unwords $ do 
             y <- [ l ..r ]
             return $ case a A.! (x,y) of
                  True -> "* " ; False -> ". "

osc :: Int -> Int -> Int -> Maybe Int
    -> SAT ( SAT [ A.Array (Int,Int) Bool ] )
osc p w h mc = do
    g0 <- relation ((1,1),(w,h))
    case mc of
         Just c -> monadic assert [ atmost c $ map snd $ assocs g0 ]
         Nothing -> return ()
    let handle 0 gs = return gs
        handle k (g:gs) = do g' <- next g ; handle (k-1) (g' : g : gs)
    gs <- handle p [ g0 ]
    forM gs bordered
    monadic assert [ equals ( head gs ) ( last gs ) ]
    forM [ d | d <- [1 .. p - 1] , 0 == mod p d ] $ \ d -> 
        monadic assert [ fmap not $ equals ( gs !! 0 ) ( gs !! d ) ]
    return $ decode $ reverse gs

bordered g = do
    let ((u,l),(d,r)) = bounds g
    forM [ u .. d ] $ \ x -> forM [ l  , r ] $ \ y -> assert [ not $ g!(x,y) ]
    forM [ u ,  d ] $ \ x -> forM [ l .. r ] $ \ y -> assert [ not $ g!(x,y) ]


next g = do
    f <- constant False
    let bnd = bounds g
    let neighbours (i,j) = do
            i' <- [ i-1, i, i+1 ]
            j' <- [ j-1, j, j+1 ]
            guard $ i /= i' Prelude.|| j /= j'
            return $ if inRange bnd (i',j') 
               then g ! (i', j')
               else f
    pairs <- forM ( assocs g ) $ \ (p, x) -> do
        y <- step x $ neighbours p
        return (p, y)
    return $ build bnd pairs

step x xs = do
    cs <- counts 3 xs
    keep <- and [ x, cs !! 2 ]
    let birth = cs !! 3
    or [ keep, birth ]
    

-- | output !! k  == True
-- if exactly  k  of the inputs are True
counts :: MonadSAT m
       => Int -> [ Boolean ] 
       -> m [ Boolean ]
counts w xs = do
    t <- constant True ; f <- constant False
    let handle cs x = do
           ds <- forM cs $ \ c -> boolean
           forM ( zip cs ds ) $ \ (c,d) -> do
               assert_fun3 ( \ c d x -> Prelude.not x <= ( c == d ) ) c d x
           forM ( zip ( f : cs) ds ) $ \ (c,d) -> do
               assert_fun3 ( \ c d x -> x <= ( c == d ) ) c d x
           return ds
    foldM handle ( t : replicate w f ) xs

    

