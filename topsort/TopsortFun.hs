module TopsortFun where

import Prelude hiding (max, lookup)
import System.Console.Terminfo (keyBackspace)
import Control.Monad (when)

import Debug.Trace ( trace )
import Text.Printf ( printf )
import qualified Data.List
import Data.Maybe ( isNothing, fromJust )

data Nat = O | S Nat deriving (Eq, Show)

instance Enum Nat where
  toEnum n | n <= 0 = O
           | otherwise = S $ toEnum $ n - 1
  fromEnum O = 0
  fromEnum (S n) = 1 + fromEnum n

less :: Nat -> Nat -> [Bool]
less x y =
  -- trace ("less\n" ++ show x ++ "\n" ++ show y ++ "\n") $
  case y of
    S y' ->
      case x of
        O -> [True]
        S x' -> less x' y'
    _ -> [False]

max :: Nat -> Nat -> [Nat]
max x y = do
  t <- less x y
  return $ if t then y else x

type Graph = [(Nat, Nat)]

type Numbering = [(Nat, Nat)]

numberOfNodes :: Graph -> [Nat]
numberOfNodes g =
    go O g
  where
    go acc g =
      case g of
        [] -> [acc]
        ((x, y) : tl) -> do
          xy <- max x y
          accxy <- max xy acc
          go accxy tl

allNums :: [Nat]
allNums = O : map S allNums

someNums :: Nat -> [Nat]
someNums n =
    reverse $ go n
  where
    go n =
      case n of
        O -> [O]
        S k -> S k : go k

lookup :: Nat -> Nat -> [(Nat, Numbering)]
lookup limit key =
  case key of
    O -> do
      x <- someNums limit
      return (x, [(O, x)])
    S k -> do
      x <- someNums limit
      (_, xs) <- lookup limit k
      return (x, (S k, x) : xs)

mergeable :: Numbering -> Numbering -> Bool
mergeable xs ys =
    all (check ys) xs && all (check xs) ys
  where
    check xs (k, v) =
      let v' = Data.List.lookup k xs
      in isNothing v' || fromJust v' == v

merge :: Numbering -> Numbering -> [Numbering]
merge xs ys | mergeable xs ys =
    go xs ys
  where
    go [] ys = [ys]
    go (x : xs) ys | x `elem` ys = map (x :) $ go xs (x `Data.List.delete` ys)
                   | otherwise = map (x :) $ go xs ys
merge _ _ = []

eval :: Graph -> [Numbering]
eval g = do
    n <- S <$> numberOfNodes g
    go g n []
  where
    go g n nums =
      case g of
        [] -> return nums
        ((b, e) : graph') -> do
          (nb, numbering) <- lookup n b
          (ne, numbering') <- lookup n e
          numbering <- merge nums numbering
          numbering <- merge numbering numbering'
          True <- less nb ne
          True <- less ne n
          numbering'' <- go graph' n numbering
          merge numbering numbering''

unf = do
  let nums = []
  let n = S (S (S O))
  let b = O
  let e = S O
  (nb, numbering) <- lookup n b
  numbering <- merge nums numbering
  (ne, numbering') <- lookup n e
  numbering <- merge numbering numbering'
  True <- less nb ne
  True <- less ne n

  let nums = numbering
  let b = O
  let e = S (S O)
  (nb, numbering) <- lookup n b
  numbering <- merge nums numbering
  (ne, numbering') <- lookup n e
  numbering <- merge numbering numbering'
  True <- less nb ne
  True <- less ne n

  let nums = numbering
  let b = S O
  let e = S (S O)
  (nb, numbering) <- lookup n b
  numbering <- merge nums numbering
  (ne, numbering') <- lookup n e
  numbering <- merge numbering numbering'
  True <- less nb ne
  True <- less ne n

  return numbering

-- eval :: Graph -> [Numbering]
-- eval g = do
--     n <- numberOfNodes g
--     go g n
--   where
--     go g n =
--       case g of
--         [] -> []
--         ((b, e) : graph') -> do
--           (nb, numbering) <- lookup n b
--           (ne, numbering') <- lookup n e
--           if trace (printf "Num:  %s\nNum': %s\n" (show numbering) (show numbering')) $ numbering == numbering'
--           then do
--             True <- less nb ne
--             True <- less ne n
--             numbering'' <- go graph' n
--             if numbering == numbering''
--             then return numbering
--             else return []
--           else return []


