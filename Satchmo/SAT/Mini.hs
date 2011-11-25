{-# LANGUAGE GeneralizedNewtypeDeriving, TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DoAndIfThenElse #-}


module Satchmo.SAT.Mini 

( SAT
, fresh
, emit
, run, run_with_timeout
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
import System.IO

newtype SAT a 
      = SAT { unSAT :: API.Solver -> IO a
            } 

instance Functor SAT where
    fmap f ( SAT m ) = SAT $ \ s -> fmap f ( m s )

instance Monad SAT where
    return x = SAT $ \ s -> return x
    SAT m >>= f = SAT $ \ s -> do x <- m s ; let { SAT n = f x } ; n s

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

run_with_timeout :: Maybe Int -> SAT (SAT a) -> IO (Maybe a)
run_with_timeout mto action = do
    accu <- newEmptyMVar 
    worker <- forkIO $ do res <- run action ; putMVar accu res
    case mto of
        to -> forkIO $ do 
          threadDelay ( 10^6 * to ) ; killThread worker ; putMVar accu Nothing
        _  -> return ()
    takeMVar accu

run :: SAT (SAT a) -> IO (Maybe a)
run ( SAT m ) = API.withNewSolver $ \ s -> do
    hPutStrLn stderr $ "start producing CNF"
    SAT decoder <- m s
    hPutStrLn stderr $ "CNF finished, starting solver"
    b <- API.solve s []
    hPutStrLn stderr $ "solver finished, result: " ++ show b
    if b then do
        hPutStrLn stderr $ "starting decoder"    
        out <- decoder s
        hPutStrLn stderr $ "decoder finished"    
        return $ Just out
    else return Nothing
