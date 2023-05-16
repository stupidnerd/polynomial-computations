module CommandActions
  ( polynomialFun,
  )
where

import Commands (UserCommand (..), showCommands)
import Data.Map (Map, empty, insert, lookup)
import Polynomial (Polynomial)
import PolynomialCalculations
  ( algebraicPolynomialExpandToDegree,
    algebraicPolynomialMult,
    algebraicPolynomialSub,
    algebraicPolynomialSumm,
    polynomialDerivative,
    printPolynomial,
    replaceVariableWithPolynomial,
    simplify
  )
import PolynomialParsing (parsePolynomial)
import Text.Read (readMaybe)
import Prelude hiding (lookup)

polynomialFun :: IO ()
polynomialFun = do
  putStrLn helloString
  userActivityLoop empty

-- | Бесконечная рекурсия обработки ввода пользовательских команд. 
-- Многочлен сохраняем в словаре (Map String Polynomial) по имени
userActivityLoop :: Map String Polynomial -> IO ()
userActivityLoop kvp = do
  putStrLn "Введите команду:"
  str <- getLine
  let value = readMaybe str
  case value of
    Nothing -> userActivityLoop kvp
    Just res -> handleCommand res kvp

-- | Обрабатываем команды, которые вводит пользователь - на каждую свое действие
handleCommand :: UserCommand -> Map String Polynomial -> IO ()
handleCommand cmd =
  case cmd of
    EnterPoly -> enterPoly
    Summ -> summPoly
    Sub -> subPoly
    Mult -> multPoly
    ToDegree -> toDegreePoly
    Replace -> replaceTerm
    Derivative -> derivativePoly
    Print -> printPoly
    Quit -> quit

-- В функциях ниже описано обращение с пользователем по вызванным им командам

-- | Ввод и сохранение многочлена.
enterPoly :: Map String Polynomial -> IO ()
enterPoly kvp = do
  putStrLn "Введите многочлен:"
  str <- getLine
  let parsingResult = parsePolynomial str
  case parsingResult of
    Left err -> do
      putStrLn err
      enterPoly kvp
    Right poly -> do
      name <- getName kvp
      let newMap = insert name poly kvp
      userActivityLoop newMap

-- | Вывод на экран сохраненного многочлена
printPoly :: Map String Polynomial -> IO ()
printPoly kvp = do
  putStrLn enterPolyName
  name <- getLine
  case lookup name kvp of
    Just val -> do
      printPolynomial val
      userActivityLoop kvp
    Nothing -> do
      putStrLn valueNotFound
      userActivityLoop kvp

-- | Функция расчета многочлена - в зависимости от второго аргумента - это может быть сумма, отрицание или умножение (см. ниже 3 функи)
calculatePoly :: Map String Polynomial -> (Polynomial -> Polynomial -> Polynomial) -> IO ()
calculatePoly kvp f = do
  putStrLn enterPolyName
  name <- getLine
  case lookup name kvp of
    Just p1 -> do
      putStrLn enterPolyName
      sname <- getLine
      case lookup sname kvp of
        Just p2 -> do
          let result = simplify $ f p1 p2
          resultName <- getName kvp
          let newMap = insert resultName result kvp
          userActivityLoop newMap
        Nothing -> do
          putStrLn valueNotFound
          userActivityLoop kvp
    Nothing -> do
      putStrLn valueNotFound
      userActivityLoop kvp

-- | Функция суммы
summPoly :: Map String Polynomial -> IO ()
summPoly kvp = calculatePoly kvp algebraicPolynomialSumm

-- | Функция вычитания
subPoly :: Map String Polynomial -> IO ()
subPoly kvp = calculatePoly kvp algebraicPolynomialSub

-- | Функция умножения
multPoly :: Map String Polynomial -> IO ()
multPoly kvp = calculatePoly kvp algebraicPolynomialMult

-- | Возведение многочлена в степень
toDegreePoly :: Map String Polynomial -> IO ()
toDegreePoly kvp = do
  putStrLn enterPolyName
  name <- getLine
  case lookup name kvp of
    Just p -> do
      e <- getExp
      let result = simplify $ algebraicPolynomialExpandToDegree p e
      resultName <- getName kvp
      let newMap = insert resultName result kvp
      userActivityLoop newMap
    Nothing -> do
      putStrLn valueNotFound
      userActivityLoop kvp

-- | Производная по заданной переменной
derivativePoly :: Map String Polynomial -> IO ()
derivativePoly kvp = do
  putStrLn enterPolyName
  name <- getLine
  case lookup name kvp of
    Just p -> do
      t <- getTerm derivativeString
      let result = simplify $ polynomialDerivative p t
      resultName <- getName kvp
      let newMap = insert resultName result kvp
      userActivityLoop newMap
    Nothing -> do
      putStrLn valueNotFound
      userActivityLoop kvp

-- | Замена переменной на многочлен
replaceTerm :: Map String Polynomial -> IO ()
replaceTerm kvp = do
  putStrLn enterPolyName
  fname <- getLine
  case lookup fname kvp of
    Just p1 -> do
      putStrLn enterPolyName
      sname <- getLine
      case lookup sname kvp of
        Just p2 -> do
          t <- getTerm replaceString
          let result = simplify $ replaceVariableWithPolynomial p1 t p2
          resultName <- getName kvp
          let newMap = insert resultName result kvp
          userActivityLoop newMap
        Nothing -> do
          putStrLn valueNotFound
          userActivityLoop kvp
    Nothing -> do
      putStrLn valueNotFound
      userActivityLoop kvp

-- | Собственно выход из программы - все наши сохранения живут только пока программа запущена или нам понадобилась бы бд.
-- Но мы пишем калькулятор, поэтому норм решение.
quit :: Map String Polynomial -> IO ()
quit _ = do 
  putStrLn "Я не сохраню твой стейт. Страдай!"

-- Ниже описаны вспомогательные функции

-- | Получение имени и проверка его в наших сохранениях - не сохраняем многочлен с именем, которое у нас уже сохранено.
getName :: Map String Polynomial -> IO String
getName kvp = do
  putStrLn "Введите имя для сохранения:"
  name <- getLine
  if isNameAlreadyExist name kvp
    then do
      putStr "Такое имя уже существует. Введите другое:"
      getName kvp
    else return name

-- | Получение степени, если пользователь ввел фигню - будет вводить до тех пор, пока не введет правильно
getExp :: IO Int
getExp = do
  putStrLn "Введите степень, в которую хотите возвести многочлен (целое число):"
  str <- getLine
  let mb = readMaybe str
  maybe getExp return mb

-- | Получение имени переменной - так же, ввел неверное значение (не одинарный символ в одинарных кавычках) - вводит заново
getTerm :: String -> IO Char
getTerm s = do
  putStrLn s
  str <- getLine
  let mb = readMaybe str
  case mb of
    Just r -> return r
    Nothing -> getTerm s

-- | Доп проверка, что имя многочлена существует в наших сохранениях
isNameAlreadyExist :: String -> Map String Polynomial -> Bool
isNameAlreadyExist key kvp = case lookup key kvp of
  Just _ -> True
  Nothing -> False

-- Дальше описаны строковые константы, которые видит пользователь.

helloString :: String
helloString =
  "Hello there!\n\
  \Эта программа предназначена для работы с многочленами.\n\
  \Достпуные команды:\n"
    ++ showCommands

enterPolyName :: String
enterPolyName = "Введите имя многочлена: "

valueNotFound :: String
valueNotFound = "По заданному имени ничего не найдено!"

derivativeString :: String
derivativeString = "Введите переменную, по которой хотите продиффернцировать многочлен:"

replaceString :: String
replaceString = "Введите переменную, которую хотите заменить на многочлен (f.e. 'x'):"