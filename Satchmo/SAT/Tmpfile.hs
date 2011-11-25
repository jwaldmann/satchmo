{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Satchmo.SAT.Tmpfile

( SAT, Header(..)
, fresh, fresh_forall
, emit, Weight
, sat
)

where

import Satchmo.Data
import Satchmo.Code
import Satchmo.Boolean
import Satchmo.Boolean.Data
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
import Data.Array
import Control.Monad.Reader

instance Decode (Reader (Array Variable Bool)) Boolean Bool where
    decode b = case b of
        Constant c -> return c
        Boolean l -> asks $ \ arr -> positive l == arr ! variable l 

instance MonadSAT SAT where
  fresh = do
    a <- get
    let n = next a
    put $ a { next = n + 1 }
    return $ literal True n
  emit clause = do
    h <- ask 
    liftIO $ hPutStrLn h $ show clause
    a <- get
    -- bshowClause c = BS.pack (show c) `mappend` BS.pack "\n"
    -- tellSat (bshowClause clause)
    put $ a
        { size = size a + 1
        , census = M.insertWith (+) (length $ literals clause) 1 $ census a 
        }
  -- emitW _ _ = return ()

  note msg = do a <- get ; put $ a { notes = msg : notes a }

  type Decoder SAT = Reader (Array Int Bool) 
  decode_variable v | v > 0 = asks $ \ arr ->  arr ! v

{-
    readsPrec p = \ cs -> do
        ( i, cs') <- readsPrec p cs
        return ( Literal i , cs' )
-}


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



tellSat x = do {h <- ask; liftIO $ BS.hPut h x}

