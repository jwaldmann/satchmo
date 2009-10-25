{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}

module Satchmo.Internal

( SAT
, fresh, fresh_forall
, emit
, sat
)

where

import Satchmo.Data

import Control.Exception
import qualified  Data.Set as Set
import Control.Monad.RWS.Strict
import qualified Data.ByteString.Lazy.Char8 as BS
import System.Directory
import System.Environment
import System.IO

data Accu = Accu
          { next :: ! Int
          , universal :: [Int]
          , size :: ! Int
          }

start :: Accu
start = Accu
      { next = 1
      , universal = []
      , size = 0
      }

type SAT a = RWST Handle () Accu IO a

sat :: SAT a -> IO (BS.ByteString, a )
sat m = bracket (getTemporaryDirectory >>= (`openTempFile`  "satchmo"))
                (\(fp, h) -> removeFile fp)
                (\(fp, h) -> do
                 hSetBuffering h (BlockBuffering Nothing)
                 ~(a, accu, _) <- runRWST m h start
                 hClose h
                 let header1 = BS.unwords [ BS.pack "p cnf"
                                         , bshow (next accu - 1)
                                         , bshow (size accu)
                                         , BS.pack "\n"]

                     existentials = [ i | i <- [1 .. next accu -1]
                                        , i `Set.notMember` Set.fromList (universal accu)]
                     universals = reverse $ universal accu

                     header2
                      | null universals = BS.empty
                      | otherwise
                      = BS.unlines
                          [ BS.pack ("e " ++ unwords (map show (existentials ++ [0])))
                          , BS.pack ("a " ++ unwords (map show (universals ++ [0])))]
                           `BS.snoc` '\n'

                 bs <- BS.readFile fp
                 return (mconcat[header1,header2, bs], a))
  where
    bshow :: Show a => a -> BS.ByteString
    bshow = BS.pack . show

-- | existentially quantified (implicitely so, before first fresh_forall)
fresh :: SAT Literal
fresh = do
    a <- get
    let n = next a
    put $ a { next = n + 1 }
    return $ literal n

-- | universally quantified
fresh_forall :: SAT Literal
fresh_forall = do
    a <- get
    let n = next a
    put $ a { next = n + 1, universal = n : universal a }
    return $ literal n

emit :: Clause -> SAT ()
emit clause = do
    a <- get
    tellSat (bshowClause clause)
    put $ a
        { size = size a + 1
        }
  where bshowClause c = BS.pack (show c) `mappend` BS.pack "\n"


tellSat x = do {h <- ask; lift $ BS.hPut h x}