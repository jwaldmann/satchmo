{-# language MultiParamTypeClasses #-}
{-# language TypeSynonymInstances #-}
{-# language FlexibleInstances #-}
{-# language NoMonomorphismRestriction #-}
{-# language TemplateHaskell #-}
{-# language DeriveGeneric #-}

module Satchmo.Boolean.Data

( Boolean(..), Booleans, encode
, boolean, exists, forall
, constant
, not, monadic
, assertOr -- , assertOrW
, assertAnd -- , assertAndW
, assert -- for legacy code
)

where

import Prelude hiding ( not )
import qualified Prelude

import qualified Satchmo.Code as C

import Satchmo.Data
import Satchmo.MonadSAT

-- import Data.Function.Memoize
import Data.Array
import Data.Maybe ( fromJust )
import Data.List ( partition )

import Control.Monad.Reader

import GHC.Generics (Generic)
import Data.Hashable

data Boolean = Boolean { encode :: !Literal }
     | Constant { value :: !Bool }
  deriving (Eq, Ord, Show, Generic)

instance Hashable Boolean

--  $(deriveMemoizable ''Boolean)

{-

-- FIXME: @Pepe: what is the reason for these instances?

instance Eq Boolean where
  b1@Boolean{}  == b2@Boolean{}  = encode b1 == encode b2
  b1@Constant{} == b2@Constant{} = value  b1 == value  b2
  _ == _ = False

instance Ord Boolean where
  b1@Boolean{}  `compare` b2@Boolean{}  = encode b1 `compare` encode b2
  b1@Constant{} `compare` b2@Constant{} = value  b1 `compare` value  b2
  Boolean{} `compare` Constant{} = GT
  Constant{} `compare` Boolean{} = LT

instance Enum Boolean where
  fromEnum (Constant True)  = -1
  fromEnum (Constant False) = 0
  fromEnum (Boolean (Literal lit) dec) = lit

  toEnum 0    = Constant False
  toEnum (-1) = Constant True
  toEnum l    = let x = literal l in Boolean x (asks $ \fm -> fromJust (M.lookup x fm))

-}

type Booleans = [ Boolean ]

isConstant :: Boolean -> Bool
isConstant ( Constant {} ) = True
isConstant _ = False


boolean :: MonadSAT m => m ( Boolean )
boolean = exists

exists :: MonadSAT m => m ( Boolean )
exists = do
    x <- fresh
    return $ Boolean 
           { encode = x
{-                      
           , decode = asks $ \ fm -> 
                      ( positive x == )
                    $ fromJust
                    $ M.lookup ( variable x ) fm
-}
           }

forall :: MonadSAT m => m ( Boolean )
forall = do
    x <- fresh_forall
    return $ Boolean 
           { encode = x
--           , decode = error "Boolean.forall cannot be decoded"
           }

constant :: MonadSAT m => Bool -> m (Boolean)
constant v = do
    return $ Constant { value = v } 
{-# INLINABLE constant #-}

-- not :: Boolean -> Boolean
not b = case b of
    Boolean {} -> Boolean 
      { encode = nicht $ encode b
      -- , decode = do x <- decode b ; return $ Prelude.not x
      }
    Constant {} -> Constant { value = Prelude.not $ value b }
{-# INLINABLE not #-}

-- assertOr, assertAnd :: MonadSAT m => [ Boolean (Literal m ) ] -> m ()
assertOr = assert

assert :: MonadSAT m => [ Boolean ] -> m ()
assert bs = do
    let ( con, uncon ) = partition isConstant bs
    let cval = Prelude.or $ map value con
    when ( Prelude.not cval ) $ emit $ clause $ map encode uncon
{-# INLINABLE assert #-}

-- assertAnd :: MonadSAT m => [ Boolean ] -> m ()
assertAnd bs = forM_ bs $ assertOr . return

{-

assertOrW, assertAndW :: MonadSAT m => Weight -> [ Boolean ] -> m ()
assertOrW w bs = do
    let ( con, uncon ) = partition isConstant bs
    let cval = Prelude.or $ map value con
    when ( Prelude.not cval ) $ emitW w $ clause $ map encode uncon

assertAndW w bs = forM_ bs $ assertOrW w . return

-}

monadic :: Monad m
        => ( [ a ] -> m b )
        -> ( [ m a ] -> m b )
monadic f ms = do
    xs <- sequence ms
    f xs

