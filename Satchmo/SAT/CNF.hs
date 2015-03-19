-- | use this module to get the actual
-- conjunctive normal form (a list of clauses).
-- You can then send this to minisat,
-- and do your own statistics and preprocessing first

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE PatternSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}


module Satchmo.SAT.CNF

( SAT
, fresh
, emit
, solve
)

where

import qualified MiniSat as API

import Satchmo.Data
import Satchmo.Boolean hiding ( not )
import Satchmo.Code
import Satchmo.MonadSAT
import Satchmo.Fourier_Motzkin ( fomo )

import Control.Monad
import Control.Monad.State.Strict
import Control.Monad.Reader

import Control.Applicative
import Control.Lens
import Data.Monoid
import Data.Foldable
import qualified Data.Map.Strict as M
import System.IO

data S = S { _next :: ! Variable
           , _output :: ! CNF
           -- , _assignment :: ! (M.Map Variable Bool)
           }

$(makeLenses ''S)

newtype SAT a = SAT { unSAT :: StateT S IO a }
  deriving ( Functor, Applicative, Monad, MonadIO, MonadState S )

instance MonadFix SAT -- dummy

instance MonadSAT SAT where
  fresh = do
      x <- get
      modify ( next %~ succ )
      return $ literal True $ x ^. next

  emit cl = do
      modify ( output %~ ( singleton cl <> ) )

  note msg = liftIO $ hPutStrLn stderr msg

  type Decoder SAT = Reader (M.Map Variable Bool)
  decode_variable v = do
      m <- ask
      return $ m M.! v
      
instance Decode (Reader (M.Map Variable Bool)) Boolean Bool where
    decode b = case b of
        Constant c -> return c
        Boolean  l -> do 
            v <- -- decode_variable $ variable l
              do m <- ask ; return $ m M.! variable l
            return $ if positive l then v else not v

solve :: SAT (Decoder SAT a) -> IO (Maybe a)
solve action = do
  (a,s) <- runStateT (unSAT action)
           $ S { _next = 1, _output = cnf [] }
  mm <- fomo $ s^.output
  return $ case mm of
    Nothing -> Nothing
    Just m -> Just $ runReader a m
    
