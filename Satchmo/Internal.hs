module Satchmo.Internal 

( SAT
, fresh, fresh_forall
, emit
, sat
)

where

import Satchmo.Data

import Control.Monad.State.Strict
import Control.Monad.Writer.Strict

data Quantified = Forall [ Int ] | Exists [ Int ]

data Accu = Accu 
          { next :: ! Int
          , quantified :: [ Quantified ]
          , size :: ! Int
          }

start :: Accu
start = Accu 
      { next = 1
      , quantified = []
      , size = 0
      }

type SAT a = WriterT [ Clause ] (State Accu) a

sat :: SAT a -> ( String, a )
sat m = 
    let ~( ~(a,w), accu) = runState ( runWriterT m ) start
    in  ( unlines $ unwords [ "p", "cnf", show ( next accu - 1), show ( size accu ) ]
                  : do q <- reverse $ interesting $ quantified accu
                       return $ case q of 
                           Forall xs -> unwords $ "a" : map show ( reverse xs ++ [0] )
                           Exists xs -> unwords $ "e" : map show ( reverse xs ++ [0] )
                  ++ map show w
        , a
        )
    
interesting [ Exists _ ] = []
interesting xs = xs

-- | existentially quantified (implicitely so, before first fresh_forall)
fresh :: SAT Literal
fresh = do
    a <- get
    let n = next a
    let q = case quantified a of
              Exists xs : rest -> Exists (n : xs) : rest
              rest -> Exists [n] : rest
    put $ a { next = n + 1, quantified = q }
    return $ literal n

-- | universally quantified
fresh_forall :: SAT Literal
fresh_forall = do
    a <- get
    let n = next a
    let q = case quantified a of
              Forall xs : rest -> Forall (n : xs) : rest
              rest -> Forall [n] : rest
    put $ a { next = n + 1, quantified = q }
    return $ literal n

emit :: Clause -> SAT ()
emit clause = do
    a <- get
    tell [ clause ]
    put $ a 
        { size = size a + 1 
        }


