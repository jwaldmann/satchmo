{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Satchmo.MonadSAT

( MonadSAT(..), Weight
, Header (..)                
)

where

import Satchmo.Data

import Control.Monad.Trans (lift)
import Control.Monad.Cont  (ContT)
import Control.Monad.List  (ListT)
import Control.Monad.Reader (ReaderT)
import qualified Control.Monad.State  as Lazy (StateT)
import qualified Control.Monad.Writer as Lazy (WriterT)
import qualified Control.Monad.RWS    as Lazy (RWST)
import qualified Control.Monad.State.Strict  as Strict (StateT)
import qualified Control.Monad.Writer.Strict as Strict (WriterT)
import qualified Control.Monad.RWS.Strict    as Strict (RWST)
import Data.Monoid

type Weight = Int

class (Functor m, Monad m) => MonadSAT m where
  fresh, fresh_forall :: m Literal
  {-# INLINE fresh #-}
  emit  :: Clause -> m ()
  {-# INLINE emit #-}
  emitW :: Weight -> Clause -> m ()

type NumClauses = Integer
type NumVars    = Integer

data Header = 
     Header { numClauses, numVars :: ! Int
            , universals :: ! [Int]
                     }
     deriving Show

-- -------------------------------------------------------
-- MonadSAT liftings for standard monad transformers
-- -------------------------------------------------------

instance (Monad m, MonadSAT m) => MonadSAT (ListT m) where
  fresh = lift fresh
  fresh_forall = lift fresh_forall
  emit  = lift . emit
  emitW = (lift.) . emitW

instance (Monad m, MonadSAT m) => MonadSAT (ReaderT r m) where
  fresh = lift fresh
  fresh_forall = lift fresh_forall
  emit  = lift . emit
  emitW = (lift.) . emitW

instance (Monad m, MonadSAT m) => MonadSAT (Lazy.StateT s m) where
  fresh = lift fresh
  fresh_forall = lift fresh_forall
  emit  = lift . emit
  emitW = (lift.) . emitW

instance (Monad m, MonadSAT m, Monoid w) => MonadSAT (Lazy.RWST r w s m) where
  fresh = lift fresh
  fresh_forall = lift fresh_forall
  emit  = lift . emit
  emitW = (lift.) . emitW

instance (Monad m, MonadSAT m, Monoid w) => MonadSAT (Lazy.WriterT w m) where
  fresh = lift fresh
  fresh_forall = lift fresh_forall
  emit  = lift . emit
  emitW = (lift.) . emitW

instance (Monad m, MonadSAT m) => MonadSAT (Strict.StateT s m) where
  fresh = lift fresh
  fresh_forall = lift fresh_forall
  emit  = lift . emit
  emitW = (lift.) . emitW

instance (Monad m, MonadSAT m, Monoid w) => MonadSAT (Strict.RWST r w s m) where
  fresh = lift fresh
  fresh_forall = lift fresh_forall
  emit  = lift . emit
  emitW = (lift.) . emitW

instance (Monad m, MonadSAT m, Monoid w) => MonadSAT (Strict.WriterT w m) where
  fresh = lift fresh
  fresh_forall = lift fresh_forall
  emit  = lift . emit
  emitW = (lift.) . emitW

instance (Monad m, MonadSAT m) => MonadSAT (ContT s m) where
  fresh = lift fresh
  fresh_forall = lift fresh_forall
  emit  = lift . emit
  emitW = (lift.) . emitW
