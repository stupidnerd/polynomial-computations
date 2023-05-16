module PolynomialCalculations
  ( simplify,
    printPolynomial,
    algebraicPolynomialSumm,
    algebraicPolynomialSub,
    algebraicPolynomialMult,
    algebraicPolynomialExpandToDegree,
    replaceVariableWithPolynomial,
    polynomialDerivative,
  )
where

import Data.List (groupBy, sortBy)
import Polynomial (Monomial (..), Polynomial (..))

-- | Многочлен представлять в виде суммы членов, включающих только операции умножения и возведения в степень.
-- В каждом таком одночлене все константы перемножены и образуют числовой коэфициент (первый сомножитель), переменные упорядочены по алфавиту и все степени одной переменной объеденены так, что каждая переменная встречается лишь один раз.
-- Следует приводить подобные члены, т.е. объединять одночлены, имеющие одинаковые наборы переменных и степеней, с соответствующим изменением коэффициентов.
simplify :: Polynomial -> Polynomial
simplify (Poly monomials) =
  Poly $ filter (not . isCoefficientZero) $ map (simplifyExponents . combineMonomials) (groupBy isSameTerms (sortBy compareMonomials monomials))

combineMonomials :: [Monomial] -> Monomial
combineMonomials monomials =
  Mono {coefficient = sum (map coefficient monomials), terms = terms (head monomials)}

simplifyExponents :: Monomial -> Monomial
simplifyExponents mono = Mono (coefficient mono) simplifiedTerms
  where
    termsGrouped = groupBy (\(var1, _) (var2, _) -> var1 == var2) $ sortBy (\(v1, _) (v2, _) -> compare v1 v2) (terms mono)
    powSum terms = sum [pow | (_, pow) <- terms]
    simplifiedTerms = [(var, powSum powGroup) | powGroup@((var, _) : _) <- termsGrouped]

compareMonomials :: Monomial -> Monomial -> Ordering
compareMonomials m1 m2 = compare (terms m1) (terms m2)

isSameTerms :: Monomial -> Monomial -> Bool
isSameTerms (Mono _ terms1) (Mono _ terms2) = terms1 == terms2

isCoefficientZero :: Monomial -> Bool
isCoefficientZero (Mono c _) = c == 0

-- | Образовать алгебраическую сумму двух многочленов.
algebraicPolynomialSumm :: Polynomial -> Polynomial -> Polynomial
algebraicPolynomialSumm (Poly []) p2 = p2
algebraicPolynomialSumm p1 (Poly []) = p1
algebraicPolynomialSumm (Poly (m1 : ms1)) (Poly (m2 : ms2)) =
  case compare (terms m1) (terms m2) of
    LT -> Poly (m1 : toList (algebraicPolynomialSumm (Poly ms1) (Poly (m2 : ms2))))
    EQ ->
      let mc = Mono (coefficient m1 + coefficient m2) (terms m1)
       in Poly (mc : toList (algebraicPolynomialSumm (Poly ms1) (Poly ms2)))
    GT -> Poly (m2 : toList (algebraicPolynomialSumm (Poly (m1 : ms1)) (Poly ms2)))
  where
    toList (Poly xs) = xs

-- | Образовать алгебраическую разность двух многочленов
algebraicPolynomialSub :: Polynomial -> Polynomial -> Polynomial
algebraicPolynomialSub = algebraicPolynomialDiff

algebraicPolynomialDiff :: Polynomial -> Polynomial -> Polynomial
algebraicPolynomialDiff (Poly p1) (Poly p2) = Poly (p1 ++ map negatePoly p2)
  where
    negatePoly (Mono c ts) = Mono (-c) ts

-- | Напечатать данный многочлен.
printPolynomial :: Polynomial -> IO ()
printPolynomial = print . simplify

-- | Образовать алгебраическое произведение двух многочленов.
algebraicPolynomialMult :: Polynomial -> Polynomial -> Polynomial
algebraicPolynomialMult (Poly []) _ = Poly []
algebraicPolynomialMult _ (Poly []) = Poly []
algebraicPolynomialMult (Poly (m : ms)) p2 =
  algebraicPolynomialSumm (polyTermMultiply m p2) (algebraicPolynomialMult (Poly ms) p2)

polyTermMultiply :: Monomial -> Polynomial -> Polynomial
polyTermMultiply _ (Poly []) = Poly []
polyTermMultiply (Mono c t) (Poly (n : ns)) =
  Poly [Mono (c * d) (sortBy sortTerms (t ++ x)) | Mono d x <- n : ns]
  where
    sortTerms (a, _) (b, _) = compare a b

-- | Возвести многочлен в степень
algebraicPolynomialExpandToDegree :: Polynomial -> Int -> Polynomial
algebraicPolynomialExpandToDegree _ 0 = Poly []
algebraicPolynomialExpandToDegree p 1 = p
algebraicPolynomialExpandToDegree p n = last $ take n $ iterate (algebraicPolynomialMult p) p

-- | Заменить каждое вхождение некоторой переменной в многочлене на данный многочлен
replaceVariableWithPolynomial :: Polynomial -> Char -> Polynomial -> Polynomial
replaceVariableWithPolynomial _ _ (Poly []) = Poly []
replaceVariableWithPolynomial (Poly source) ch insert = Poly $ helper source ch insert
  where
    helper :: [Monomial] -> Char -> Polynomial -> [Monomial]
    helper [] _ _ = []
    helper (s : ss) ch' insert' = handleMonomial s ch insert ++ helper ss ch' insert'

handleMonomial :: Monomial -> Char -> Polynomial -> [Monomial]
handleMonomial m ch p =
  if isContainCharOccurance m ch
    then
      let deleted = deleteCharOccurance m ch
          inserted = insertPolynomial deleted p
       in unPoly inserted
    else [m]

isContainCharOccurance :: Monomial -> Char -> Bool
isContainCharOccurance (Mono _ terms) = helper' terms
  where
    helper' [] _ = False
    helper' ((var, _) : ts) ch = (var == ch) || helper' ts ch

unPoly :: Polynomial -> [Monomial]
unPoly (Poly ms) = ms

insertPolynomial :: (Maybe Monomial, (Float, Int)) -> Polynomial -> Polynomial
insertPolynomial (Nothing, (coef, exp')) p = algebraicPolynomialMult (coefToPoly coef) $ algebraicPolynomialExpandToDegree p exp'
insertPolynomial (Just m, (_, exp')) p = algebraicPolynomialMult (monoToPoly m) $ algebraicPolynomialExpandToDegree p exp'

coefToPoly :: Float -> Polynomial
coefToPoly n = Poly [Mono n []]

monoToPoly :: Monomial -> Polynomial
monoToPoly m = Poly [m]

deleteCharOccurance :: Monomial -> Char -> (Maybe Monomial, (Float, Int))
deleteCharOccurance (Mono coef terms) ch =
  let termsAfterDeletion = newTerms terms ch
   in (if (not . null) termsAfterDeletion then Just $ Mono coef termsAfterDeletion else Nothing, (coef, deletedTermExp ch terms))
  where
    newTerms :: [(Char, Int)] -> Char -> [(Char, Int)]
    newTerms [] _ = []
    newTerms [t] ch' = [t | ch' /= fst t]
    newTerms (t : ts) ch' = if ch' == fst t then newTerms ts ch' else t : newTerms ts ch'
    deletedTermExp :: Char -> [(Char, Int)] -> Int
    deletedTermExp ch' = snd . head . filter (\(x, _) -> x == ch')

-- | Вычислить производную многочлена по переменной
polynomialDerivative :: Polynomial -> Char -> Polynomial
polynomialDerivative (Poly []) _ = Poly []
polynomialDerivative (Poly (mono : monos)) v =
  case lookup v (terms mono) of
    Nothing -> polynomialDerivative (Poly monos) v
    Just 0 -> polynomialDerivative (Poly monos) v
    Just n ->
      let coef = coefficient mono
          term = filter (\t -> fst t /= v) (terms mono)
       in Poly [Mono (coef * fromIntegral n) ((v, n - 1) : term)] <> polynomialDerivative (Poly monos) v