{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Satchmo.SAT.Tmpfile

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

-- import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.ByteString.Char8 as BS

import System.Directory
import System.Environment
import System.IO

import qualified Data.Map as M

import Data.List ( sortBy )
import Data.Ord ( comparing )

instance MonadSAT SAT where
  fresh = satfresh
  fresh_forall = satfresh_forall
  emit    = satemit
  emitW _ _ = return ()
  note msg = do a <- get ; put $ a { notes = msg : notes a }

-- ---------------
-- Implementation
-- ---------------

data Accu = Accu
          { next :: ! Int
          , universal :: [Int]
          , size :: ! Int
          , notes :: ! [ String ]
          , census :: ! ( M.Map Int Int )
          }

start :: Accu
start = Accu
      { next = 1
      , universal = []
      , size = 0
      , notes = [ "Satchmo.SAT.Tmpfile implementation" ]
      , census = M.empty          
      }

newtype SAT a = SAT {unsat::RWST Handle () Accu IO a}
    deriving (MonadState Accu, MonadReader Handle, Monad, MonadIO, Functor)


sat :: SAT a -> IO (BS.ByteString, Header, a )
sat (SAT m) =
 bracket
    (getTemporaryDirectory >>= (`openTempFile`  "satchmo"))
    (\(fp, h) -> removeFile fp)
    (\(fp, h) -> do
       hSetBuffering h (BlockBuffering Nothing)
       ~(a, accu, _) <- runRWST m h start
       hClose h
       
       forM ( reverse $ notes accu ) $ hPutStrLn stderr 
       hPutStrLn stderr $ unlines 
           [ "(clause length, frequency)"
           , show $ sortBy ( comparing ( negate . snd )) 
                        $ M.toList $ census accu
           ]  
       
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
    return $ literal True n

-- | universally quantified
satfresh_forall :: SAT Literal
satfresh_forall = do
    a <- get
    let n = next a
    put $ a { next = n + 1, universal = n : universal a }
    return $ literal True n

satemit :: Clause -> SAT ()
satemit clause = do
    h <- ask ; liftIO $ hPutStrLn h $ show clause
    a <- get
    -- tellSat (bshowClause clause)
    put $ a
        { size = size a + 1
        , census = M.insertWith (+) (length $ literals clause) 1 $ census a         
        }
  where bshowClause c = BS.pack (show c) `mappend` BS.pack "\n"


tellSat x = do {h <- ask; liftIO $ BS.hPut h x}

