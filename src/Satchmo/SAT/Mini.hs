{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}


module Satchmo.SAT.Mini 

( SAT
, fresh
, emit
, SolveOptions(..)
, defaultSolveOptions
, solve
, solveSilently
, solveWith
, solve_with_timeout
)

where

import qualified MiniSat as API

import Satchmo.Data
import Satchmo.Boolean hiding ( not )
import Satchmo.Code
import Satchmo.MonadSAT

import Control.Concurrent
import Control.Concurrent.MVar
import Control.Exception
import Control.Monad ( when )
import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Applicative
import System.IO

import Control.Concurrent.Async

deriving instance Enum API.Lit

newtype SAT a 
      = SAT { unSAT :: API.Solver -> IO a
            } 

instance Functor SAT where
    fmap f ( SAT m ) = SAT $ \ s -> fmap f ( m s )

instance Monad SAT where
    return x = SAT $ \ s -> return x
    SAT m >>= f = SAT $ \ s -> do 
        x <- m s ; let { SAT n = f x } ; n s

-- | need this for hashtables
instance MonadIO SAT where
  liftIO comp = SAT $ \ s -> comp

instance Applicative SAT where
    pure = return
    a <*> b = a >>= \ f -> fmap f b

instance MonadFix SAT where
    mfix f = SAT $ \ s -> mfix ( \ a -> unSAT (f a) s )

instance MonadSAT SAT where
  fresh = SAT $ \ s -> do 
      x <- API.newLit s
      let l = literal True $ fromEnum x
      -- hPutStrLn stderr $ "fresh: " ++ show (x, l)
      return l

  emit cl = SAT $ \ s -> do
      let conv l = ( if positive l then id else API.neg ) 
                 $ toEnum
                 $ variable l
          apicl = map conv $ literals cl
      res <- API.addClause s apicl
      -- hPutStrLn stderr $ "adding clause " ++ show (cl, apicl, res)
      return ()

  note msg = SAT $ \ s -> hPutStrLn stderr msg

  type Decoder SAT = SAT 
  decode_variable v = SAT $ \ s -> do
      Just val <- API.modelValue s $ toEnum $ fromEnum v
      return val 
      
instance Decode SAT Boolean Bool where
    decode b = case b of
        Constant c -> return c
        Boolean  l -> do 
            let dv v = SAT $ \ s -> do
                    Just val <- API.modelValue s $ toEnum $ fromEnum v
                    return val 
            v <- dv $ variable l
            return $ if positive l then v else not v

newtype SolveOptions = SolveOptions {
        verboseOutput :: Bool
    }

defaultSolveOptions :: SolveOptions
defaultSolveOptions = SolveOptions {verboseOutput = True}

solve_with_timeout :: Maybe Int -> SAT (SAT a) -> IO (Maybe a)
solve_with_timeout mto action = do
    accu <- newEmptyMVar 
    worker <- forkIO $ do res <- solve action ; putMVar accu res
    timer <- forkIO $ case mto of
        Just to -> do 
              threadDelay ( 10^6 * to ) 
              killThread worker 
              putMVar accu Nothing
        _  -> return ()
    takeMVar accu `Control.Exception.catch` \ ( _ :: AsyncException ) -> do
        hPutStrLn stderr "caught"
        killThread worker
        killThread timer
        return Nothing

solve :: SAT (SAT a) -> IO (Maybe a)
solve = solveWith defaultSolveOptions

solveSilently :: SAT (SAT a) -> IO (Maybe a)
solveSilently = solveWith defaultSolveOptions{verboseOutput = False}

solveWith :: SolveOptions -> SAT (SAT a) -> IO (Maybe a)
solveWith options action = withNewSolverAsync $ \ s -> do
    let printIfVerbose = when (verboseOutput options) . hPutStrLn stderr
    printIfVerbose "start producing CNF"
    SAT decoder <- unSAT action s
    v <- API.minisat_num_vars s
    c <- API.minisat_num_clauses s
    printIfVerbose $ unwords [ "CNF finished", "vars", show v, "clauses", show c ]
    printIfVerbose "starting solver"
    status <- API.limited_solve s []
    printIfVerbose $ "solver finished, result: " ++ show status
    if status == API.l_True then do
        printIfVerbose "starting decoder"    
        out <- decoder s
        printIfVerbose "decoder finished"    
        return $ Just out
    else return Nothing


withNewSolverAsync h =
  bracket newSolver API.deleteSolver $ \  s -> do
    mask_ $ withAsync (h s) $ \ a -> do
      wait a `onException` API.minisat_interrupt s

newSolver =
  do s <- API.minisat_new
     -- https://github.com/niklasso/minisat-haskell-bindings/issues/6
     -- eliminate s True 
     return s
