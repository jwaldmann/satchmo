{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Satchmo.SAT.BS

( SAT, Header(..)
, fresh, fresh_forall
, emit, Weight
, sat
)

where

import Satchmo.Data
import qualified Satchmo.Data.Default as D
import Satchmo.MonadSAT

import Control.Exception

import Control.Monad.RWS.Strict
-- import Control.Monad.RWS.Lazy

-- import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.ByteString.Char8 as BS

import Data.Foldable
import Data.String

newtype SAT a 
      = SAT { unSAT :: -- WriterT BS.ByteString ( StateT Header Identity ) 
                       -- StateT Header ( WriterT BS.ByteString Identity )
                       RWS () BS.ByteString Header
                       -- RWS () ( S.Seq BS.ByteString ) Header
                       a
            } 
      deriving ( Functor, Monad
               , MonadState Header
               , MonadWriter BS.ByteString
               -- , MonadWriter ( S.Seq BS.ByteString )
               )


instance MonadSAT SAT where
  type Literal SAT = D.Literal
  fresh = do 
      a <- get 
      let v = numVars a
      put $ a { numVars = succ v } 
      return $ literal True $ succ v
  emit cl = do
      a <- get
      put $ a { numClauses = succ $ numClauses a }
      tell $  fromString $ show  cl ++ "\n"
  note msg = do
      return ()
     
start :: Header
start = Header { numClauses = 0, numVars = 0
               , universals = []
               }


sat :: SAT a -> IO (BS.ByteString, Header, a )
sat (SAT m) = do
    let -- ((res,bs),accu) = runState (runWriterT m) start
        -- ((res,accu),bs) = runWriter ( runStateT m  start )
        (res, accu, bs) = runRWS m () start 
    return ( bs , accu, res )


