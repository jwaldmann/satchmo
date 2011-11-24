-- | This is the API for plugging in solver implementations.
-- Actual implementations are in the (separate) package @satchmo-backends@

module Satchmo.Solve

( solve 
, Implementation
-- , solveW
-- , WeightedImplementation
)

where

import Satchmo.Data
import qualified Satchmo.Data.Default as D
import Satchmo.Code
import Satchmo.SAT

-- import qualified Satchmo.SAT.Weighted as Weighted

-- import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.ByteString.Char8 as BS

import Data.Map ( Map )
import qualified Data.Map as M
import Data.Array
import Control.Monad.Reader

import System.IO

type Implementation = BS.ByteString 
                    -> Header 
                    -> IO ( Maybe ( Array Variable Bool ) )


solve :: Implementation 
      -> SAT ( Reader ( Array Variable Bool ) a ) 
      -> IO ( Maybe a )
solve implementation build = do
    (s, h, a) <- sat build
    mfm <- implementation s h
    case mfm of
        Nothing -> do
            hPutStrLn stderr "not satisfiable"
            return Nothing
        Just fm -> do
            hPutStrLn stderr "satisfiable"
            return $ Just $ runReader a fm

{-

type WeightedImplementation = BS.ByteString 
    -> Weighted.Header 
    -> IO ( Maybe ( Array Variable Bool ) )

solveW :: Weighted.MaxWeight 
       -> WeightedImplementation 
       -> Weighted.SAT (Decoder a) 
       -> IO (Maybe a)
solveW maxW implementation build = do
    (s, h, a) <- Weighted.sat maxW build
    mfm <- implementation s h
    case mfm of
        Nothing -> do
            hPutStrLn stderr "not satisfiable"
            return Nothing
        Just fm -> do
            hPutStrLn stderr "satisfiable"
            return $ Just $ runReader a fm

-}