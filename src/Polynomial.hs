{-# LANGUAGE InstanceSigs #-}

module Polynomial (Polynomial (..), Monomial (..)) where

import Data.List (intercalate)

data Monomial = Mono {coefficient :: Float, terms :: [(Char, Int)]}
  deriving Eq

newtype Polynomial = Poly [Monomial]
  deriving Eq

instance Show Monomial where
  show :: Monomial -> String
  show (Mono c ts) =
    if showC c == "" then showC c ++ showTerms else showC c ++ " * " ++ showTerms
    where
      showC c'
        | c' == 1.0 = ""
        | c' < 0 = "(" ++ show c' ++ ")"
        | otherwise = show c'
      showT (_, 0) = ""
      showT (x, 1) = [x]
      showT (x, e) = [x] ++ "^" ++ show e
      showTerms = intercalate " * " (filter (not . (==) "") (map showT ts))

instance Show Polynomial where
  show :: Polynomial -> String
  show (Poly monomials) = intercalate " + " $ map show monomials

instance Semigroup Polynomial where
  (<>) :: Polynomial -> Polynomial -> Polynomial
  (<>) (Poly []) (Poly []) = Poly []
  (<>) p (Poly []) = p
  (<>) (Poly []) p = p
  (<>) (Poly p1) (Poly p2) = Poly (p1 <> p2)
  
instance Monoid Polynomial where
  mempty :: Polynomial
  mempty = Poly []
  mappend :: Polynomial -> Polynomial -> Polynomial
  mappend = (<>)

  