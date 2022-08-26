-- | Simple Pigoenhole benchmark:
-- put  p  pigeons in (p-1) holes.

{-# language ScopedTypeVariables #-}

import Prelude hiding ( not, product )
import qualified Prelude

import qualified Satchmo.Counting as C
import Satchmo.Code
import Satchmo.Boolean

import Satchmo.SAT.Mini

import Data.Maybe (isJust)
import Data.List (transpose)
import Control.Monad ( replicateM, forM_ )
import System.Environment

main :: IO ()
main = do
    argv <- getArgs
    case argv of
      [ ] -> main_with 10
      [s] -> main_with $ read s

main_with :: Int -> IO ()
main_with n = do
    s <- solve $ pigeon n
    print $ isJust s

pigeon :: Int -> SAT (SAT ())
pigeon p = do
  xss <- replicateM p $ replicateM (p-1) boolean
  forM_             xss $ \ xs -> assertM $ C.atleast 1 xs
  forM_ (transpose xss) $ \ ys -> assertM $ C.atmost  1 ys
  return $ decode ()

assertM action = do x <- action ; assert [x]

