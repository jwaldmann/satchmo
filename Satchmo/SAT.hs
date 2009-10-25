{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Satchmo.SAT

( SAT, Header(..)
, fresh, fresh_forall
, emit, Weight
, sat
)

where

import Satchmo.Data
import Satchmo.MonadSAT

import Control.Exception
import Control.Monad.RWS.Strict
import qualified  Data.Set as Set
import qualified Data.ByteString.Lazy.Char8 as BS
import System.Directory
import System.Environment
import System.IO


instance MonadSAT SAT where
  fresh = satfresh
  fresh_forall = satfresh_forall
  emit    = satemit
  emitW _ _ = return ()

-- ---------------
-- Implementation
-- ---------------

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

newtype SAT a = SAT {unsat::RWST Handle () Accu IO a}
    deriving (MonadState Accu, MonadReader Handle, Monad, MonadIO, Functor)

type NumClauses = Integer
type NumVars    = Integer

data Header = Header { numClauses, numVars :: Int
                     , universals :: [Int]
                     }


sat :: SAT a -> IO (BS.ByteString, Header, a )
sat (SAT m) =
 bracket
    (getTemporaryDirectory >>= (`openTempFile`  "satchmo"))
    (\(fp, h) -> removeFile fp)
    (\(fp, h) -> do
       hSetBuffering h (BlockBuffering Nothing)
       ~(a, accu, _) <- runRWST m h start
       hClose h
       let header = Header (size accu) (next accu - 1) universals
           universals = reverse $ universal accu

       bs <- BS.readFile fp
       return (bs, header, a))

-- | existentially quantified (implicitely so, before first fresh_forall)
satfresh :: SAT Literal
satfresh = do
    a <- get
    let n = next a
    put $ a { next = n + 1 }
    return $ literal n

-- | universally quantified
satfresh_forall :: SAT Literal
satfresh_forall = do
    a <- get
    let n = next a
    put $ a { next = n + 1, universal = n : universal a }
    return $ literal n

satemit :: Clause -> SAT ()
satemit clause = do
    a <- get
    tellSat (bshowClause clause)
    put $ a
        { size = size a + 1
        }
  where bshowClause c = BS.pack (show c) `mappend` BS.pack "\n"


tellSat x = do {h <- ask; liftIO $ BS.hPut h x}

