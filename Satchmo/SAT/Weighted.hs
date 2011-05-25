{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Satchmo.SAT.Weighted (SAT, sat, MaxWeight, Header(..)) where

import Satchmo.Data
import Satchmo.MonadSAT hiding ( Header )

import Control.Exception
import Control.Monad.RWS.Strict
import Data.Maybe
import qualified  Data.Set as Set
import qualified Data.ByteString.Lazy.Char8 as BS
import System.Directory
import System.Environment
import System.IO


instance MonadSAT SAT where
  fresh = satfresh
  fresh_forall = satfresh_forall
  emit  = satemit Nothing
  emitW = satemit . Just

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

type MaxWeight  = Int

newtype SAT a = SAT {unsat::RWST (Handle, MaxWeight) () Accu IO a}
    deriving (MonadState Accu, MonadReader (Handle, MaxWeight), Monad, MonadIO, Functor)

data Header = Header { numClauses, numVars, maxWeight :: Int
                     , universals :: [Int]
                     }

sat :: MaxWeight -> SAT a -> IO (BS.ByteString, Header, a )
sat maxW (SAT m) =
 bracket
    (getTemporaryDirectory >>= (`openTempFile`  "satchmo"))
    (\(fp, h) -> removeFile fp)
    (\(fp, h) -> do
       hSetBuffering h (BlockBuffering Nothing)
       ~(a, accu, _) <- runRWST m (h, maxW) start
       hClose h
       let header = Header (size accu) (next accu - 1) maxW universals
           universals = reverse $ universal accu

       bs <- BS.readFile fp
       return (bs, header, a))

-- | existentially quantified (implicitely so, before first fresh_forall)
satfresh :: SAT Literal
satfresh = do
    a <- get
    let n = next a
    put $ a { next = n + 1 }
    return $ literal True n

-- | universally quantified
satfresh_forall :: SAT Literal
satfresh_forall = do
    a <- get
    let n = next a
    put $ a { next = n + 1, universal = n : universal a }
    return $ literal True n

satemit :: Maybe Weight -> Clause -> SAT ()
satemit w (Clause clause) = do
    a <- get
    (h,maxW) <- ask
    liftIO $ BS.hPut h (bshowClause $ Clause(Literal (fromMaybe maxW w) : clause))
    put $ a { size = size a + 1}

  where bshowClause c = BS.pack (show c) `mappend` BS.pack "\n"

