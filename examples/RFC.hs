-- rectangle free colourings of grids
-- see http://www.cs.umd.edu/~gasarch/papers/grid.pdf

{-# language ScopedTypeVariables #-}
{-# language NoMonomorphismRestriction #-}

import Prelude hiding ( not, and, or, product )
import qualified Prelude

import Satchmo.Boolean
import Satchmo.Code
import qualified Satchmo.Counting as C

import qualified Satchmo.Relation as R

import Satchmo.SAT.Mini (solve)

import Data.List (sort, tails)
import qualified Data.Array as A
import Control.Monad ( guard, when, forM, foldM, void )
import Control.Applicative ((<$>))
import System.Environment
import Data.Ix ( range)
import qualified Data.List

main :: IO ()
main = do
     [ col, wid, hei ] <- getArgs
     Just (rs :: [ A.Array (Int,Int) Bool ]) <- 
         solve $ rfc (read col :: Int) (read wid :: Int, read hei :: Int)
     void $ forM rs printA

rfc col (w,h) = do
    
    rs <- if col == 4 Prelude.&& w == h 
          then do
              r <- R.relation ((1,1),(w,h))
              return $ take 4 $ iterate rotate r
          else if col == 4 
          then do
              [r0,r1] <- sequence $ replicate 2 $ R.relation ((1,1),(w,h))
              return $ [r0, r1, rotate $ rotate r0, rotate $ rotate r1 ] 
          else forM [1..col] $ \ c -> R.relation ((1,1),(w,h))

    -- mustbe_balanced col rs

    f <- constant False
    let trans = if odd w Prelude.&& odd h then drop_middle f else id
    -- mustbe $ disjoint $ map trans rs

    assert_complete rs

    let rs' = if col == 4 Prelude.&& w == h then take 1 rs else rs
    assert_no_rectangle_tricky rs' (w,h)


    return $ decode rs

assert_no_rectangle_tricky rs (w,h) = void $ forM rs $ \ r -> do
    -- for each row, the list of ands of pairs
    ps <- forM (columns r) $ \ col ->
                forM (selections 2 col) $ \ [p,q] -> and [p,q]
    void $ forM (Data.List.transpose ps) $ \ bs -> 
        assert_atmostone bs

assert_atmostone bs = mustbe $ C.atmost 1 bs
assert_atmostone' bs = void $ forM (selections 2 bs) $ \cs -> assert $ map not cs


selections k xs | k > length xs = []
selections 0 xs = return []
selections k (x:xs) = 
    map (x:) (selections (k-1) xs) ++ selections k xs
        
drop_middle f r = 
    let b @ ((1,1),(w,h)) = R.bounds r
        m = (1+div w 2, 1 + div h 2)
    in  R.build b $ map ( \(p,b) -> (p, if p == m then f else b)) $ R.assocs r

mustbe_balanced col rs = void $ forM rs $ \ r -> do
    let ((1,1),(w,h)) = R.bounds r
    void $ forM (rows r) $ \ row -> bal col row
    void $ forM (columns r) $ \ colm -> bal col colm

bal col xs = do
    let lo = div (length xs) col 
        hi = div (length xs + col - 1) col
    mustbe $ C.atleast  lo xs
    -- mustbe $ C.atmost   hi xs

rows r = do
    let ((1,1),(w,h)) = R.bounds r
    x <- [ 1 .. w ]
    return $ do y <- [1 .. h] ; return $ r R.! (x,y)

columns r = do
    let ((1,1),(w,h)) = R.bounds r
    y <- [ 1 .. h ]
    return $ do x <- [1 .. w] ; return $ r R.! (x,y)

rotate r = 
    let ((1,1),(w,h)) = R.bounds r
    in  R.build ((1,1),(h,w)) $ do
            ((x,y),a) <- R.assocs r
            return ((y, w+1-x), a)
flipX r = 
    let b @ ((1,1),(w,h)) = R.bounds r
    in  R.build b $ do
            ((x,y),a) <- R.assocs r
            return ((w+1-x,y), a)
flipY r = 
    let b @ ((1,1),(w,h)) = R.bounds r
    in  R.build b $ do
            ((x,y),a) <- R.assocs r
            return ((x,h+1-y), a)

assert_no_rectangle rs (w,h) = do
    void $ forM rs $ \ r ->
           forM [1 .. w-1] $ \ a -> forM [a+1 .. w] $ \ b ->
           forM [1 .. h-1] $ \ c -> forM [c+1 .. h] $ \ d -> do
             assert $ do
                 x <- [a, b]; y <- [c, d]
                 return $ not $ r R.! (x,y)

---------------------------------------------------------------------------

disjoint = disjoint0

disjoint0 [] = constant True
disjoint0 (x:ys) = do
    here <- forM ys $ \ y -> R.disjoint x y
    there <- disjoint0 ys
    and $ there : here

disjoint1 rs = do
    oks <- forM (R.indices $ head rs) $ \ i -> do
        sequence $ do
            x : ys <- tails $ map (R.! i) rs
            y <- ys
            return $ not <$> and [ x, y ]
    and $ concat oks
        
---------------------------------------------------------------------------

complete = complete1

complete1 rs = do
    oks <- forM (R.indices $ head rs) $ \ i -> or $ map ( R.! i ) rs
    and oks

complete0 (r : rs) = do
    u <- foldM R.union r rs
    R.complete u

assert_complete rs = do
    void $ forM (R.indices $ head rs) $ \ i -> assert $ map ( R.! i ) rs

---------------------------------------------------------------------------

mustbe f = do x <- f ; assert [ x ]

printA :: A.Array (Int,Int) Bool -> IO ()
printA a = putStrLn $ unlines $ do
         let ((u,l),(o,r)) = A.bounds a
         x <- [u .. o]
         return $ unwords $ do 
             y <- [ l ..r ]
             return $ case a A.! (x,y) of
                  True -> "* " ; False -> ". "


