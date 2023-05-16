module PolynomialParsing
  ( parsePolynomial,
    parseMonomial,
  )
where

import Polynomial (Monomial (Mono), Polynomial (..))
import Text.Parsec
  ( Parsec,
    char,
    digit,
    letter,
    many1,
    optional,
    runParser,
    sepBy,
    spaces,
    try,
    (<|>)
  )
import Prelude hiding (exp, exponent)

-- | Разбирает мономиальное выражение из указанной входной строки и возвращает соответствующий объект Monomial.
-- Если разбор прошел успешно, возвращает объект Monomial. Если разбор не удался, сгенерирует ошибку. parseMonomial :: String -> Monomial parseMonomial input = either (error . show) id (runParser monomialParser () "" input)
parseMonomial :: String -> Either String Monomial
parseMonomial input = case runParser monomialParser () "" input of
  Left err -> Left (show err)
  Right res -> Right res

-- | Код определяет парсер, используя библиотеку Parsec в Haskell. Он называется monomialParser и парсит строку в моном, который является математическим выражением, состоящим из коэффициента и одной или более переменных, возведенных в определенные степени.
-- Парсер сначала пытается разобрать плавающий коэффициент, используя функцию parseFloat. Если это не удалось, он пытается разобрать целочисленный коэффициент, используя функцию parseInt. Если оба разбора не удалось, то коэффициент по умолчанию равен 1.0.
-- После разбора коэффициента парсер пропускает все пробелы, а затем опционально разбирает символ '*', за которым может следовать еще пробел.
-- Затем он разбирает одну или несколько переменных и их степеней, используя парсер term, разделенные символами '*' и пробелами.
-- Наконец, он конструирует и возвращает значение Monomial, используя коэффициент и список пар (переменная, степень), которые были разобраны ранее.
-- Точкой входа парсера является monomialParser. Он использует несколько вспомогательных парсеров, определенных с использованием конструкции do и комбинаторов Parsec, таких как try, option, <*>, и <|>.
monomialParser :: Parsec String () Monomial
monomialParser = do
  coeff <- try parseFloat <|> try parseInt <|> pure 1.0
  spaces >> optional (char '*') >> spaces
  ts <- term `sepBy` (spaces >> char '*' >> spaces)
  return $ Mono coeff ts
  where
    term = do
      var <- letter
      spaces >> optional (char '^') >> spaces
      exp <- try (read <$> many1 digit) <|> pure 1
      spaces
      return (var, exp)
    parseFloat = do
      optional (char '(')
      sign <- try (char '-') <|> pure '+'
      f <- many1 digit
      dot <- char '.'
      s <- many1 digit
      optional (char ')')
      if sign == '+'
        then
          return $ read (f ++ [dot] ++ s)
        else
          return $ read ([sign] ++ f ++ [dot] ++ s)
    parseInt = do
      optional (char '(')
      sign <- try (char '-') <|> pure '+'
      n <- many1 digit
      optional (char ')')
      if sign == '+'
        then
          return $ read n
        else
          return $ read (sign : n)

-- | Функция polynomialParser определяет парсер для разбора многочлена в виде строки и возвращает тип Polynomial. Она начинает с вызова другого парсера, monomialParser, который парсит отдельные мономы многочлена.
-- Этот парсер разделен знаком + и пробелами с использованием функции sepBy и разбирается в список мономов ts. Наконец, функция создает тип данных Polynomial из списка мономов с помощью конструктора Poly и возвращает его.
polynomialParser :: Parsec String () Polynomial
polynomialParser = do
  ts <- monomialParser `sepBy` (spaces >> char '+' >> spaces)
  return $ Poly ts

-- | Функция parsePolynomial принимает строковый ввод и возвращает многочлен.
-- Аргумент: input: строка, представляющая выражение полиномиальной функции.
-- Возвращает тип данных Polynomial
parsePolynomial :: String -> Either String Polynomial
parsePolynomial input = case runParser polynomialParser () "" input of
  Left err -> Left (show err)
  Right result -> Right result