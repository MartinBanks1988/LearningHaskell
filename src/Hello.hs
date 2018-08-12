{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -W -Werror #-}

module Lib where

import           Control.Monad ((<=<), (>=>))
import           Data.Foldable (foldlM)
import           Data.Functor.Foldable (Fix(..), cata)
import qualified Data.Map.Strict as M
import           Safe (readMay)

data ASTF a
  = LitF Int
  | AddF a a
  | SubF a a
  | MulF a a
  | DivF a a
  | AbsF a
  deriving (Show, Functor, Foldable, Traversable)

type AST = Fix ASTF

lit :: Int -> AST
lit = Fix . LitF

add, sub, mul, div_ :: AST -> AST -> AST
add a b = Fix (AddF a b)
sub a b = Fix (SubF a b)
mul a b = Fix (MulF a b)
div_ a b = Fix (DivF a b)

abs_ :: AST -> AST
abs_ = Fix . AbsF

eval :: AST -> Maybe Int
eval = cataM alg where
  alg (LitF i) = Just $ i
  alg (AddF a b) = Just $ a + b
  alg (SubF a b) = Just $ a - b
  alg (MulF a b) = Just $ a * b
  alg (DivF a b) = a `safeDiv` b where
    safeDiv _ 0 = Nothing
    safeDiv a b = Just $ a `div` b
  alg (AbsF a) = Just $ abs a

  cataM = cata . (sequence >=>)

unaryOperators :: M.Map String (AST -> AST)
unaryOperators = M.fromList
  [ ("abs", abs_)
  ]

binaryOperators :: M.Map String (AST -> AST -> AST)
binaryOperators = M.fromList
  [ ("+", add)
  , ("-", sub)
  , ("*", mul)
  , ("/", div_)
  ]

parse :: String -> Maybe AST
parse = singleton <=< foldlM parseToken [] . words where
  parseToken s mi | Just i <- readMay mi = Just $ (lit i):s
  parseToken (a:s) mo | Just o <- unaryOperators M.!? mo = Just $ (o a):s
  parseToken (b:a:s) mo | Just o <- binaryOperators M.!? mo = Just $ (o a b):s
  parseToken _ _ = Nothing

  singleton :: [a] -> Maybe a
  singleton [a] = Just a
  singleton _ = Nothing
