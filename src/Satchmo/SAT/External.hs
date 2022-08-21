{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE PatternSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# language TemplateHaskell #-}

-- | call an external solver as  separate process,
-- communicate via pipes.

module Satchmo.SAT.External

( SAT
, fresh
, emit
, solve
-- , solve_with_timeout
)

where

import Satchmo.Data
import Satchmo.Boolean hiding ( not )
import Satchmo.Code
-- import Satchmo.MonadSAT

import Control.Monad.Reader
import Control.Monad.State
-- import Control.Monad.IO.Class
import System.IO
import Control.Lens
import Control.Applicative

import Control.Concurrent
import Control.DeepSeq (rnf)

import Foreign.C
-- import System.Exit (ExitCode(..))
import System.Process
-- import System.IO.Error
-- import System.Posix.Types
import Control.Exception
import GHC.IO.Exception ( IOErrorType(..), IOException(..) )
-- import System.Posix.Signals

import qualified Control.Exception as C
import qualified Data.ByteString.Char8 as BS
import qualified Data.Map.Strict as M
import Data.List (isPrefixOf)

tracing = False
report s = when tracing $ hPutStrLn stderr s

data S = S
       { _next_variable :: !Int 
       , _solver_input :: !Handle 
       }

$(makeLenses ''S)

newtype SAT a = SAT (StateT S IO a)
  deriving (Functor, Applicative, Monad, MonadIO)

type Assignment = M.Map Int Bool

newtype Dec a = Dec (Reader Assignment a)
  deriving (Functor, Applicative, Monad)

instance MonadSAT SAT where
  fresh = SAT $ do 
      n <- use next_variable
      next_variable .= succ n
      return $ literal True $ fromEnum n
  emit cl = SAT $ do
      h <- use solver_input
      let s = BS.pack $ show cl
      -- liftIO $ BS.putStrLn s
      liftIO $ BS.hPutStrLn h s 

  note msg = SAT $ liftIO $ hPutStrLn stderr msg

  type Decoder SAT = Dec

instance Decode Dec Boolean Bool where
    decode b = case b of
        Constant c -> return c
        Boolean  l -> do
            v <- dv $ variable l 
            return $ if positive l then v else not v

dv v = Dec $ do 
  assignment <- ask
  return $ case M.lookup v assignment of
    Just v -> v
    Nothing -> error $ unwords [ "unassigned", "variable", show v ]
      

solve :: String  -- ^ command, e.g., glucose
      -> [String] -- ^ options, e.g., -model
      -> SAT (Dec a) -- ^ action that builds the formula and returns the decoder
      -> IO (Maybe a)
solve command opts (SAT action) = bracket
   ( do
     report "Satchmo.SAT.External: creating process"
     createProcess $ (proc command opts) 
       { std_in = CreatePipe 
       , std_out = CreatePipe
       , create_group = True 
       } )
   ( \ (Just sin, Just sout, _, ph) -> do
       report "Satchmo.SAT.External: bracket closing"
       interruptProcessGroupOf ph
   )
   $ \ (Just sin, Just sout, _, ph) -> do

       dec <- newEmptyMVar

       -- fork off a thread to start consuming the output
       output  <- hGetContents sout -- lazy IO
       withForkWait (C.evaluate $ rnf output) $ \ waitOut -> 
          ignoreSigPipe $ do
            report $ "S.S.External: waiter forked"

            let s0 = S { _next_variable=1, _solver_input=sin}
            report $ "S.S.External: writing output"
            Dec decoder <- evalStateT action s0
            putMVar dec decoder
            hClose sin

            waitOut
            hClose sout
            report $ "S.S.External: waiter done"

       report "Satchmo.SAT.External: start waiting"
       waitForProcess ph
       decoder <- takeMVar dec
       report "Satchmo.SAT.External: waiting done"

       let vlines = do
             line <- lines output
             guard $ isPrefixOf "v" line
             return line
       report $ show vlines
       let vs = do
             line <- vlines
             w <- tail $ words line
             return (read w :: Int)
       return $ do
         guard $ not $ null vlines
         let m = M.fromList $ do 
               v <- vs ; guard $ v /= 0 ; return (abs v, v>0)
         return $ runReader decoder m

-- * code from System.Process 
-- http://hackage.haskell.org/package/process-1.2.3.0/docs/src/System-Process.html#readProcess
-- but they are not exporting withForkWait, so I have to copy it

-- | Fork a thread while doing something else, but kill it if there's an
-- exception.
--
-- This is important in the cases above because we want to kill the thread
-- that is holding the Handle lock, because when we clean up the process we
-- try to close that handle, which could otherwise deadlock.
--
withForkWait :: IO () -> (IO () ->  IO a) -> IO a
withForkWait async body = do
  waitVar <- newEmptyMVar :: IO (MVar (Either SomeException ()))
  mask $ \restore -> do
    tid <- forkIO $ try (restore async) >>= putMVar waitVar
    let wait = takeMVar waitVar >>= either throwIO return
    restore (body wait) `C.onException` killThread tid

ignoreSigPipe :: IO () -> IO ()
ignoreSigPipe = C.handle $ \e -> case e of
  IOError { ioe_type  = ResourceVanished
          , ioe_errno = Just ioe }
    | Errno ioe == ePIPE -> return ()
  _ -> throwIO e
