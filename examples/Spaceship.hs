-- | compute spaceship for Conway's game of life, 
-- cf. http://www.conwaylife.com/wiki/Category:Oscillators
-- arguments are: distanceX, distanceY, max. period
-- width [, height, [number of life start cells]]
-- example usage: 
-- ./Spaceship 1 1 4 6     -- glider
-- ./Spaceship 0 2 4 7 9 9 -- Conway's lightweight spaceship

{-# language PatternSignatures #-}
{-# language FlexibleContexts #-}

import Prelude hiding ( not, or, and )
import qualified Prelude

import Satchmo.Relation
import Satchmo.Code
import Satchmo.Boolean hiding ( equals, implies )
import qualified Satchmo.Binary as B

import qualified Satchmo.Counting.Direct as CD
import qualified Satchmo.Counting.Unary as CU
import qualified Satchmo.Counting.Binary as CB

import Satchmo.SAT.Mini

import Data.List (sort)
import qualified Data.Array as A
import Control.Monad ( guard, when, forM, foldM
  , void, replicateM )
import System.Environment
import Data.Ix ( range, inRange )

main :: IO ()
main = void $ do
    argv <- getArgs
    Just gs <- case map read argv of
        [] ->
          solve $ glide 0  2  4 7 9 (Just 9)
        [ dx, dy, p, w       ] ->
          solve $ glide dx dy p w w Nothing
        [ dx, dy, p, w, h    ] ->
          solve $ glide dx dy p w h Nothing
        [ dx, dy, p, w, h, c ] ->
          solve $ glide dx dy p w h $ Just c
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

for = flip map

glide :: Int -> Int -> Int -> Int -> Int -> Maybe Int
      -> SAT ( SAT [A.Array (Int,Int) Bool] )
glide dx dy p w h mc = do
    g0 <- relation ((1,1),(w,h))
    assert $ map snd $ assocs g0
    case mc of
         Just c -> monadic assert [ CB.atmost c $ map snd $ assocs g0 ]
         Nothing -> return ()
    let handle 0 g = return [g]
        handle k g = do g' <- next g ; gs <- handle (k-1) g' ; return $ g : gs
    gs <- handle p g0 
    forM gs bordered

    ms <- forM ( tail gs ) $ \ h -> moved (dx,dy) ( head gs ) h
    assert $ ms

    return $ decode gs

equals r s = monadic and [ implies r s, implies s r ]

moved (dx,dy) g h = do
    f <- constant False
    let bnd @ ((l,o),(r,u)) = bounds g
        get g p = if inRange bnd p then g ! p else f
    monadic and $ for ( range bnd ) $ \ (x,y) -> do
        fun2 (==) ( get g (x,y) ) ( get h (x+dx, y+dy) )


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

step = step_mod

step_mod x xs = do
  c <- CB.count xs
  drei <- B.constant 3
  birth <- B.equals drei c
  zwei <- B.constant 2
  keep <- B.equals zwei c
  keepx <- and [keep, x]
  or [ keepx, birth ]

step_orig x xs = do
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

    

