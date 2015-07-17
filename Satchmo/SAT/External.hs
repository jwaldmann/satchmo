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
import Satchmo.MonadSAT

import Control.Monad.Reader
import Control.Monad.State
import System.IO
import Control.Lens
import Control.Applicative

import System.Process
import Control.Exception (bracket)
import qualified Data.ByteString.Char8 as BS
import qualified Data.Map.Strict as M

data S = S
       { _next_variable :: ! Int 
       , _solver_input :: ! Handle 
       }

$(makeLenses ''S)

newtype SAT a = SAT (StateT S IO a)
  deriving (Functor, Applicative, Monad)

type Assignment = M.Map Int Bool

newtype Dec a = Dec (Reader Assignment a)
  deriving Monad  

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
      
tracing = False
report s = when tracing $ hPutStrLn stderr s

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
       let s0 = S { _next_variable=1, _solver_input=sin}
       Dec decoder <- evalStateT action s0
       hClose sin
       report "Satchmo.SAT.External: start waiting"
       waitForProcess ph
       report "Satchmo.SAT.External: waiting done"
       out <- liftIO $ BS.hGetContents sout
       let vlines = do
             line <- BS.lines out
             guard $ BS.isPrefixOf (BS.pack "v") line
             return line
       report $ show vlines
       let vs = do
             line <- vlines
             w <- tail $ BS.words line
             return (read $ BS.unpack w :: Int)
       return $ do
         guard $ not $ null vlines
         let m = M.fromList $ do 
               v <- vs ; guard $ v /= 0 ; return (abs v, v>0)
         return $ runReader decoder m

  