{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Satchmo.SAT.Seq

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
-- import Control.Monad.RWS.Lazy

-- import qualified Data.Sequence as S
import qualified Satchmo.SAT.Sequence as S

import qualified Data.ByteString.Char8 as BS
-- import qualified Data.ByteString.Lazy.Char8 as BS

import Data.Foldable
import Data.String

newtype SAT a 
      = SAT { unSAT :: -- WriterT BS.ByteString ( StateT Header Identity ) 
                       -- StateT Header ( WriterT BS.ByteString Identity )
                       -- RWS () BS.ByteString Header
                       RWS () ( S.Seq BS.ByteString ) Header
                       a
            } 
      deriving ( Functor, Monad
               , MonadState Header
               , MonadWriter ( S.Seq BS.ByteString )
               -- , MonadWriter BS.ByteString
               )


instance MonadSAT SAT where
  note msg = do return ()
  fresh = do 
      a <- get 
      let v = numVars a
      put $ a { numVars = succ v } 
      return $ literal True $ succ v
  emit cl = do
      a <- get
      put $ a { numClauses = succ $ numClauses a }
      tell $  S.singleton $ fromString $ show  cl ++ "\n"

     
start :: Header
start = Header { numClauses = 0, numVars = 0
               , universals = []
               }


sat :: SAT a -> IO (BS.ByteString, Header, a )
sat (SAT m) = do
    let -- ((res,bs),accu) = runState (runWriterT m) start
        -- ((res,accu),bs) = runWriter ( runStateT m  start )
        (res, accu, bs) = runRWS m () start 
    return ( fold bs , accu, res )


