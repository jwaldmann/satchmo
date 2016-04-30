{-# language TemplateHaskell #-}
{-# language LambdaCase #-}

module Satchmo.TH ( compile ) where

import qualified Satchmo.Boolean as B

import Language.Haskell.TH
import Control.Applicative

compile m = m >>= sat

lifted2 op a b = do x <- a ; y <- b ; op x y

sat e = case e of
  InfixE (Just l) (VarE op) (Just r)
    | show op == "GHC.Classes.&&" ->
      [| lifted2 (B.&&) $(sat l)  $(sat r) |]
    | show op == "GHC.Classes.||" ->
      [| lifted2 (B.||)  $(sat l)  $(sat r) |]
    | show op == "GHC.Classes.==" ->
      [| lifted2 B.equals2  $(sat l)  $(sat r) |]
    | show op == "GHC.Classes./=" ->
      [| lifted2 B.xor2  $(sat l)  $(sat r) |]
  AppE (VarE op) arg
    | show op == "GHC.Classes.not" ->
      [| B.not <$> $(sat arg) |]
  VarE n -> [| return $(return e) |]
  ConE c
    | show c == "GHC.Types.False" ->
      [| B.constant False |]
    | show c == "GHC.Types.True" ->
      [| B.constant True |]
  _ -> error $ "Satchmo.TH.sat: missing case for " ++ show e

  
