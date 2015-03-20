{-# language TupleSections #-}

module Satchmo.Fourier_Motzkin where

import Satchmo.Data

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Control.Monad ( guard )
import Data.Monoid
import Data.List ( maximumBy, minimumBy )
import Data.Function (on)
import System.IO

type Solver = CNF -> IO (Maybe (M.Map Variable Bool))

fomo :: Solver
fomo cnf = do
  print_info "fomo" cnf
  ( remove_satisfied $ trivial $ onesided $  eliminate $ branch ) cnf

print_info msg cnf = do
  hPutStrLn stderr $ unwords [ msg, show $ size cnf, "\n" ]
  -- hPutStrLn stderr $ show cnf ++ "\n"

remove_satisfied cont cnf = do
  -- print_info "remove_satisfied" cnf
  let vars polar cl = S.fromList $ do
        lit <- literals cl;
        guard $ positive lit == polar
        return $ variable lit
      remaining = Satchmo.Data.filter
        ( \ cl -> disjoint ( vars True cl ) ( vars False cl ))
        cnf
  cont cnf

trivial :: Solver -> Solver
trivial cont cnf = do
  -- print_info "trivial" cnf
  if null $ clauses cnf
     then return $ Just M.empty
     else if clause [] `elem` clauses cnf
          then return $ Nothing
          else cont cnf

onesided :: Solver -> Solver
onesided cont cnf = do
  -- print_info "onesided" cnf
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
  -- hPutStrLn stderr $ unwords [ "assigned", show assigned , "\n" ]       
  later <- ( if size others < size cnf then fomo else cont ) others
  return $ fmap ( M.union assigned ) later

disjoint s t = S.null $ S.intersection s t

eliminate :: Solver -> Solver
eliminate cont nf = do
  print_info "eliminate" nf
  let pos = occurrences True  nf
      neg = occurrences False nf
      reductions = M.intersectionWith
         ( \ xs ys -> let lx = length xs
                          ly = length xs
                      in  lx*ly - lx - ly
         ) pos neg
      resolve v = splitAt 10000 $ do -- ARBITRARY NUMBER
        cp <- pos M.! v
        let cpv = cp `without` v
        cn <- neg M.! v
        let cnv = cn `without` v
        let c = cpv <> cnv
        guard $ c /= CTrue
        return $ c
      others v = Satchmo.Data.filter
        ( \ cl -> not $ elem v $ map variable $ literals cl )
        nf
      reconstruct v m = Prelude.or $ do
        cp <- pos M.! v
        return $ Prelude.not $ Prelude.or $ do
          lit <- literals $ cp `without` v
          let v = M.findWithDefault False ( variable lit ) m
          return $ if positive lit then v else Prelude.not v 
      (v,c) = minimumBy (compare `on` snd) $ M.toList reductions
  hPutStrLn stderr $ unwords [ "best resolution:", show v, "count", show c ]
  let ( pre,post) = resolve v
  if null post
    then do
               hPutStrLn stderr $ unwords [ "do it" ]
               later <- fomo $ others v <> cnf pre
               return $ fmap
                    ( \ m -> M.insert v (reconstruct v m) m)
                    later
    else do
               hPutStrLn stderr $ unwords [ "do not do it" ]
               cont nf

branch cnf = do
  print_info "branch" cnf
  let stat = M.fromListWith (+) $ do
        c <- clauses cnf
        let ls = literals c
        let w = 1 / fromIntegral (length ls)
        l <- ls
        return (variable l, w)
      (v,w) = maximumBy (compare `on` snd) $ M.toList stat
  hPutStrLn stderr $ unwords [ "on variable", show (v,w), "False" ]
  a <- fomo $ assign v False cnf
  case a of
    Just m -> return $ Just $ M.insert v False m
    Nothing -> do
      hPutStrLn stderr $ unwords [ "on variable", show (v,w), "True" ]
      b <- fomo $ assign v True cnf
      return $ fmap (M.insert v True) b
        
-- | map each var to list of clauses where it occurs 
occurrences :: Bool -> CNF -> M.Map Variable [Clause]
occurrences polarity  =
  flip Satchmo.Data.foldr M.empty $ \ cl ->
    M.unionWith (++) $ M.fromList $ do
      lit <- literals cl
      guard $ positive lit == polarity
      return ( variable lit, [cl] )
