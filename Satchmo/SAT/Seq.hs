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
import Control.Monad.Writer
import Control.Monad.State.Strict

import qualified Data.ByteString.Lazy.Char8 as BS
import Data.String

newtype SAT a 
      = SAT { unSAT :: WriterT BS.ByteString
                       ( State Header ) a
            } 
      deriving ( Functor, Monad
               , MonadState Header
               , MonadWriter BS.ByteString ) 


instance MonadSAT SAT where
  fresh = do 
      a <- get 
      let v = numVars a
      put $ a { numVars = succ v } 
      return $ literal True $ succ v
  emit cl = do
      a <- get
      put $ a { numClauses = succ $ numClauses a }
      tell $ fromString $ show  cl 
      tell $ fromString "\n"

     
start :: Header
start = Header { numClauses = 0, numVars = 0
               , universals = []
               }


sat :: SAT a -> IO (BS.ByteString, Header, a )
sat (SAT m) = do
    let ((res,bs),accu) = 
            runState (runWriterT m) start
    return ( bs, accu, res )


