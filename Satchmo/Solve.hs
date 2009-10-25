-- | This is the API for plugging in solver implementations.
-- Actual implementations are in the (separate) package @satchmo-backends@

module Satchmo.Solve

( solve
, Implementation
, Decoder
)

where

import Satchmo.Data
import Satchmo.Code
import Satchmo.Internal

import Data.Map ( Map )
import qualified Data.Map as M

import Control.Monad.State
import Control.Monad.Reader

type Implementation = String -> IO ( Maybe ( Map Literal Bool ) )

solve :: Implementation
      -> SAT ( Decoder a )
    -> IO ( Maybe a )
solve implementation build = do
    let (s, a) = sat build
    mfm <- implementation s
    case mfm of
        Nothing -> do
            putStrLn "not satisfiable"
            return Nothing
        Just fm -> do
            putStrLn "satisfiable"
            -- print fm
            return $ Just $ runReader a fm
                
