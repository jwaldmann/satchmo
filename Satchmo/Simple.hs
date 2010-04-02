{-# language GeneralizedNewtypeDeriving #-}

module Satchmo.Simple where

import Satchmo.MonadSAT
import Satchmo.Data

import Control.Monad.State

data Accu = Accu { next :: ! Int
                 , pool :: [ Clause ]
                 }

start = Accu { next = 0, pool = [] }

sat (SAT m) = flip evalState start
            $ do x <- m; a <- get ; return (cnf $ pool a, x) 

newtype SAT a = SAT { unsat :: State Accu a } 
    deriving ( Functor, Monad )

instance MonadSAT SAT where
    fresh = SAT $ do 
          a <- get ; let n = succ $ next a 
          put $ a { next = n } ; return $ Literal n
    emit c = SAT $ do
          modify $ \ a -> a { pool = c : pool a }


        