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
    powSum terms' = sum [pow | (_, pow) <- terms']
    simplifiedTerms = [(var, powSum powGroup) | powGroup@((var, _) : _) <- termsGrouped]

compareMonomials :: Monomial -> Monomial -> Ordering
compareMonomials m1 m2 = compare (terms m1) (terms m2)

isSameTerms :: Monomial -> Monomial -> Bool
isSameTerms (Mono _ terms1) (Mono _ terms2) = terms1 == terms2

isCoefficientZero :: Monomial -> Bool
isCoefficientZero (Mono c _) = c == 0

-- | Образовать алгебраическую сумму двух многочленов. Принимает два многочлена в качестве входных данных и возвращает их сумму в формате многочлена.
-- Для реализации функции используется сопоставление с образцом для входных многочленов, чтобы рассмотреть случаи, когда один или оба многочлена являются пустым Poly []. 
-- Если p1 - пустой многочлен, результатом будет p2. Если p2 - пустой, результатом будет p1. 
-- Если оба многочлена не пустые, код выполняет следующее сравнение: compare (terms m1) (terms m2), где m1 и m2 - ведущие мономы двух многочленов, соответственно.
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

-- | Образовать алгебраическую разность двух многочленов. Код выполняет алгебраическое дифференцирование многочленов, вычитая второй многочлен из первого многочлена.
-- Результирующий многочлен возвращается в качестве выходных данных. Функция использует сопоставление с образцом для извлечения коэффициентов и членов двух входных многочленов.
-- Оба извлеченных многочлена объединяются, поэтому на второй многочлен применяется функция "negatePoly" для инвертирования коэффициентов.
-- Функция "map" используется для применения функции "negatePoly" к каждому члену второго многочлена. Функция "negatePoly" принимает многочлен (Mono) в качестве входных данных и возвращает новый многочлен с инвертированным коэффициентом.
-- Результирующий многочлен после вычитания и инвертирования возвращается как новый многочлен с помощью конструктора "Poly".
algebraicPolynomialSub :: Polynomial -> Polynomial -> Polynomial
algebraicPolynomialSub = algebraicPolynomialDiff

algebraicPolynomialDiff :: Polynomial -> Polynomial -> Polynomial
algebraicPolynomialDiff (Poly p1) (Poly p2) = Poly (p1 ++ map negatePoly p2)
  where
    negatePoly (Mono c ts) = Mono (-c) ts

-- | Напечатать данный многочлен.
printPolynomial :: Polynomial -> IO ()
printPolynomial = print . simplify

-- | Образовать алгебраическое произведение двух многочленов. Первые два случая функции служат базовыми случаями для рекурсии функции.
-- Если один из входных полиномов пуст (его список коэффициентов пуст), то функция возвращает пустой полином. Третий случай - это рекурсивный случай.
-- Он умножает первый термин входного полинома (представленный m) на второй входной полином p2.
-- Для этого используется функция polyTermMultiply, которая умножает один полиномный терм на другой полином и возвращает результирующий полином.
-- Результатирующий полином добавляется к произведению оставшихся терминов первого полинома, умноженных на второй полином.
-- Таким образом, функция рекурсивно перемножает полиномы термин за термином, пока не будут выполнены базовые случаи.
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

-- | Возвести многочлен в степень. Функция принимает два параметра: 
-- Многочлен, представленный в виде списка кортежей, где первый элемент кортежа - это коэффициент слагаемого, а второй элемент - это степень. Например, многочлен 3x^2 + 4x - 2 будет представлен в виде [(3,2),(4,1),(-2,0)].
-- Целое число, представляющее желаемую степень. Функция использует сопоставление с образцом для обработки двух базовых случаев:
-- Если желаемая степень равна 0, то возвращается многочлен с пустым списком, представляющим нулевой многочлен. Если желаемая степень равна 1, то возвращается исходный многочлен без изменений.
-- Для всех остальных случаев функция использует функцию iterate для многократного умножения исходного многочлена сам на себя, используя функцию algebraicPolynomialMult.
-- Затем используется функция take для извлечения многочлена желаемой степени. Наконец, последняя функция используется для получения желаемого многочлена из конца списка сгенерированных многочленов.
-- В целом, эта функция представляет собой простой алгоритм расширения многочлена до определённой степени, используя только операции умножения.
algebraicPolynomialExpandToDegree :: Polynomial -> Int -> Polynomial
algebraicPolynomialExpandToDegree _ 0 = Poly []
algebraicPolynomialExpandToDegree p 1 = p
algebraicPolynomialExpandToDegree p n = last $ take n $ iterate (algebraicPolynomialMult p) p

-- | Заменить каждое вхождение некоторой переменной в многочлене на данный многочлен. Функция принимает три аргумента:
-- Исходный многочлен (Polynomial типа), символ для замены (Char типа), многочлен для вставки (Polynomial типа). Она возвращает объект Polynomial.
-- Для реализации, если данный многочлен для вставки пуст, то возвращается пустой многочлен. В противном случае, функция берет исходный многочлен и передает его во вспомогательную функцию с именем helper.
-- Функция helper проходит по списку мономов в многочлене. Для каждого монома в списке, вспомогательная функция вызывает другую функцию с именем handleMonomial,
-- которая принимает текущий моном (s) вместе с символом, который нужно заменить (ch) и многочленом для вставки (insert).
-- Функция handleMonomial возвращает список мономов, где символ ch заменен на заданный многочлен insert. Наконец, обновленный список мономов конкатенируется с текущим многочленом путем рекурсивного вызова функции-помощника.
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
isContainCharOccurance (Mono _ terms') = helper' terms'
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
deleteCharOccurance (Mono coef terms') ch =
  let termsAfterDeletion = newTerms terms' ch
   in (if (not . null) termsAfterDeletion then Just $ Mono coef termsAfterDeletion else Nothing, (coef, deletedTermExp ch terms'))
  where
    newTerms :: [(Char, Int)] -> Char -> [(Char, Int)]
    newTerms [] _ = []
    newTerms [t] ch' = [t | ch' /= fst t]
    newTerms (t : ts) ch' = if ch' == fst t then newTerms ts ch' else t : newTerms ts ch'
    deletedTermExp :: Char -> [(Char, Int)] -> Int
    deletedTermExp ch' = snd . head . filter (\(x, _) -> x == ch')

-- | Вычислить производную многочлена по переменной. Принимает полином (Polynomial), который является списком мономов (Monos) и символ (Char), представляющий переменную.
-- Он возвращает производную входного полинома по заданной переменной. Если переменная не присутствует в каком-либо мономе ввода, функция возвращает пустой полином.
-- Полином деструктурируется с помощью сопоставления с образцом для извлечения первого монома в списке (mono) и оставшихся мономов (monos). Если список пуст, возвращается пустой полином.
-- Функция lookup используется для поиска переменной в терминах извлеченного монома. Если она отсутствует или если коэффициент равен 0, функция вызывается рекурсивно со списком оставшихся мономов до тех пор, пока не будет найдено совпадение.
-- Если происходит совпадение, коэффициент монома умножается на показатель степени переменной и вычитается 1, чтобы создать новый показатель. Новый моном строится с этими значениями, объединенными в 2-кортеж.
-- Функция filter используется для удаления термина, соответствующего переменной, из списка терминов монома. Новый моном затем добавляется к окончательному результату и объединяется с рекурсивно вычисляемой производной оставшихся мономов в списке с помощью конструктора Poly.
-- Итоговый результат - новый полином, представляющий производную по заданной переменной.
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