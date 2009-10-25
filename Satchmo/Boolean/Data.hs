{-# language MultiParamTypeClasses #-}

module Satchmo.Boolean.Data 

( Boolean, Booleans
, boolean, exists, forall
, constant
, not, assert, monadic
)

where

import Prelude hiding ( not )
import qualified Prelude

import qualified Satchmo.Code as C

import Satchmo.Data 
import Satchmo.Internal

import Data.Map ( Map )
import qualified Data.Map as M
import Data.Maybe ( fromJust )
import Data.List ( partition )

import Control.Monad.Reader

data Boolean = Boolean
             { encode :: Literal
             , decode :: C.Decoder Bool
             }
     | Constant { value :: Bool }

type Booleans = [ Boolean ]

isConstant :: Boolean -> Bool
isConstant ( Constant {} ) = True
isConstant _ = False

instance C.Decode Boolean Bool where 
    decode b = case b of
        Boolean {} -> decode b
        Constant {} -> return $ value b

boolean :: SAT Boolean
boolean = exists

exists :: SAT Boolean
exists = do
    x <- fresh
    return $ Boolean 
           { encode = x
           , decode = asks $ \ fm -> fromJust $ M.lookup x fm
           }

forall :: SAT Boolean
forall = do
    x <- fresh_forall
    return $ Boolean 
           { encode = x
           , decode = error "Boolean.forall cannot be decoded"
           }

constant :: Bool -> SAT Boolean
constant v = do
    return $ Constant { value = v } 

not :: Boolean -> Boolean
not b = case b of
    Boolean {} -> Boolean 
      { encode = nicht $ encode b
      , decode = do x <- decode b ; return $ Prelude.not x
      }
    Constant {} -> Constant { value = Prelude.not $ value b }

assert :: [ Boolean ] -> SAT ()
assert bs = do
    let ( con, uncon ) = partition isConstant bs
    let cval = Prelude.or $ map value con
    when ( Prelude.not cval ) $ emit $ clause $ map encode uncon

monadic :: Monad m
        => ( [ a ] -> m b )
        -> ( [ m a ] -> m b )
monadic f ms = do
    xs <- sequence ms
    f xs

