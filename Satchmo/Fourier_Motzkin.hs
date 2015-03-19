{-# language TupleSections #-}

module Satchmo.Fourier_Motzkin where

import Satchmo.Data

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Control.Monad ( guard )
import Data.Monoid
import Data.List ( sortBy, nub )
import Data.Function (on)
import System.IO

type Solver = CNF -> IO (Maybe (M.Map Variable Bool))

fomo :: Solver
fomo cnf = do
  print_info cnf
  ( trivial $ onesided $  eliminate fomo ) cnf

print_info cnf = do
  hPutStrLn stderr $ show cnf

trivial :: Solver -> Solver
trivial cont cnf = do
  if null $ clauses cnf
     then return $ Just M.empty
     else if clause [] `elem` clauses cnf
          then return $ Nothing
          else cont cnf

onesided :: Solver -> Solver
onesided cont cnf = do
  let pos = occurrences True  cnf
      neg = occurrences False cnf
      onlypos = M.keys $ M.difference pos neg
      onlyneg = M.keys $ M.difference neg pos
      assigned = M.fromList
          $ map (,True) onlypos ++ map (,False) onlyneg
      ks = M.keysSet assigned
      others = Satchmo.Data.filter
         ( \ cl -> disjoint ks
                   $ S.fromList $ map variable $ literals cl) 
         cnf
  later <- cont others
  return $ fmap ( M.union assigned ) later

disjoint s t = S.null $ S.intersection s t

eliminate :: Solver -> Solver
eliminate cont nf = do
  let pos = occurrences True  nf
      neg = occurrences False nf
      reductions = M.intersectionWith
         ( \ xs ys -> let lx = length xs
                          ly = length xs
                      in  negate (lx*ly - lx - ly)
         ) pos neg
      resolve v = cnf $ do
        cp <- pos M.! v
        let cpv = cp `without` (literal True  v) 
        cn <- neg M.! v
        let cnv = cn `without` (literal False v) 
        guard $ disjoint ( S.fromList (literals cpv))
           (S.fromList (literals cnv))
        return $  cpv <> cnv
      others v = Satchmo.Data.filter
        ( \ cl -> not $ elem v $ map variable $ literals cl )
        nf
      reconstruct v m = Prelude.or $ do
        cp <- pos M.! v
        return $ Prelude.not $ Prelude.and $ do
          lit <- literals cp
          guard $ lit /= literal True v
          let v = m M.! variable lit
          return $ if positive lit then v else Prelude.not v
  case sortBy (compare `on` snd) $ M.toList reductions of
        (v,c): _ -> do
           later <- cont $ others v <> resolve v
           return $ fmap
                    ( \ m -> M.insert v (reconstruct v m) m)
                    later

-- | map each var to list of clauses where it occurs 
occurrences :: Bool -> CNF -> M.Map Variable [Clause]
occurrences polarity  =
  flip Satchmo.Data.foldr M.empty $ \ cl ->
    M.unionWith (++) $ M.fromList $ do
      lit <- literals cl
      guard $ positive lit == polarity
      return ( variable lit, [cl] )
