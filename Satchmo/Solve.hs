-- | This is the API for plugging in solver implementations.
-- Actual implementations are in the (separate) package @satchmo-backends@

module Satchmo.Solve

( solve, solveW
, Implementation
, Decoder
)

where

import Satchmo.Data
import Satchmo.Code
import Satchmo.SAT
import qualified Satchmo.SAT.Weighted as Weighted

import qualified Data.ByteString.Lazy.Char8 as BS
import Data.Map ( Map )
import qualified Data.Map as M

import Control.Monad.Reader

type Implementation = BS.ByteString -> Header -> IO ( Maybe ( Map Literal Bool ) )
type WeightedImplementation = BS.ByteString -> Weighted.Header -> IO ( Maybe ( Map Literal Bool ) )

solve :: Implementation -> SAT ( Decoder a ) -> IO ( Maybe a )
solve implementation build = do
    (s, h, a) <- sat build
    mfm <- implementation s h
    case mfm of
        Nothing -> do
            putStrLn "not satisfiable"
            return Nothing
        Just fm -> do
            putStrLn "satisfiable"
            return $ Just $ runReader a fm

solveW :: Weighted.MaxWeight -> WeightedImplementation -> Weighted.SAT (Decoder a) -> IO (Maybe a)
solveW maxW implementation build = do
    (s, h, a) <- Weighted.sat maxW build
    mfm <- implementation s h
    case mfm of
        Nothing -> do
            putStrLn "not satisfiable"
            return Nothing
        Just fm -> do
            putStrLn "satisfiable"
            return $ Just $ runReader a fm
