{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Satchmo.Internal

( SAT, MonadSAT(..)
, fresh, fresh_forall
, emit
, sat
)

where

import Satchmo.Data

import Control.Exception
import qualified  Data.Set as Set
import Control.Monad.Cont (ContT)
import Control.Monad.List (ListT)
import Control.Monad.Reader (ReaderT)
import qualified Control.Monad.State  as Lazy (StateT)
import qualified Control.Monad.Writer as Lazy (WriterT)
import qualified Control.Monad.RWS    as Lazy (RWST)
import qualified Control.Monad.State.Strict  as Strict (StateT)
import qualified Control.Monad.Writer.Strict as Strict (WriterT)
import Control.Monad.RWS.Strict              as Strict
import qualified Data.ByteString.Lazy.Char8 as BS
import System.Directory
import System.Environment
import System.IO

class (Functor m, Monad m) => MonadSAT m where
  fresh, fresh_forall :: m Literal
  emit :: Clause -> m ()

instance MonadSAT SAT where
  fresh = satfresh
  fresh_forall = satfresh_forall
  emit  = satemit

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


sat :: SAT a -> IO (BS.ByteString, a )
sat (SAT m) =
        bracket (getTemporaryDirectory >>= (`openTempFile`  "satchmo"))
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
                 return (mconcat[header1, bs], a))
  where
    bshow :: Show a => a -> BS.ByteString
    bshow = BS.pack . show

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


-- -------------------------------------------------------
-- MonadSAT liftings for standard monad transformers
-- -------------------------------------------------------

instance (Monad m, MonadSAT m) => MonadSAT (ListT m) where
  fresh = lift fresh
  fresh_forall = lift fresh_forall
  emit = lift . emit

instance (Monad m, MonadSAT m) => MonadSAT (ReaderT r m) where
  fresh = lift fresh
  fresh_forall = lift fresh_forall
  emit = lift . emit

instance (Monad m, MonadSAT m) => MonadSAT (Lazy.StateT s m) where
  fresh = lift fresh
  fresh_forall = lift fresh_forall
  emit = lift . emit

instance (Monad m, MonadSAT m, Monoid w) => MonadSAT (Lazy.RWST r w s m) where
  fresh = lift fresh
  fresh_forall = lift fresh_forall
  emit = lift . emit

instance (Monad m, MonadSAT m, Monoid w) => MonadSAT (Lazy.WriterT w m) where
  fresh = lift fresh
  fresh_forall = lift fresh_forall
  emit = lift . emit

instance (Monad m, MonadSAT m) => MonadSAT (Strict.StateT s m) where
  fresh = lift fresh
  fresh_forall = lift fresh_forall
  emit = lift . emit

instance (Monad m, MonadSAT m, Monoid w) => MonadSAT (Strict.RWST r w s m) where
  fresh = lift fresh
  fresh_forall = lift fresh_forall
  emit = lift . emit

instance (Monad m, MonadSAT m, Monoid w) => MonadSAT (Strict.WriterT w m) where
  fresh = lift fresh
  fresh_forall = lift fresh_forall
  emit = lift . emit

instance (Monad m, MonadSAT m) => MonadSAT (ContT s m) where
  fresh = lift fresh
  fresh_forall = lift fresh_forall
  emit = lift . emit
