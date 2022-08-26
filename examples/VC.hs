-- | command line arguments: n s
-- compute vertex cover of size <= s for knight's graph on  n x n  chess board.
-- (that is, if you put knights there, they control the full board)
-- example: VC 8 12

{-# language ScopedTypeVariables #-}

import Prelude hiding ( not )

import Satchmo.Relation
import Satchmo.Code
import Satchmo.Boolean
import Satchmo.Counting

import Satchmo.SAT.Mini

import Control.Monad ( guard )
import System.Environment
import System.Timeout
import qualified Data.Array as A

main :: IO ()
main = do
    argv <- getArgs
    let [ n, s ] = map read argv
    Just ( a :: A.Array (Int,Int) Bool ) <- solve $ knight n s
    putStrLn $ table a

knight n s = do
    a <- relation ((1,1),(n,n))
    m <- atmost s $ do 
           i <- indices a ; return $ a ! i
    assert [m]
    sequence_ $ do
        p <- indices a
        return $ assert $ do
            q <- indices a
            guard $ p == q Prelude.|| reaches p q
            return $ a!q
    return $ decode a
        
reaches (px,py) (qx,qy) = 
    5 == (px - qx)^2 + (py - qy)^2

    
