module Part2.Tasks where

import Util(notImplementedYet)

data BinaryOp = Plus | Minus | Times deriving (Show, Eq)

data Term = IntConstant { intValue :: Int }          -- числовая константа
          | Variable    { varName :: String }        -- переменная
          | BinaryTerm  { op :: BinaryOp, lhv :: Term, rhv :: Term } -- бинарная операция
             deriving(Show,Eq)

-- Для бинарных операций необходима не только реализация, но и адекватные
-- ассоциативность и приоритет
(|+|) :: Term -> Term -> Term
(|+|) = notImplementedYet
(|-|) :: Term -> Term -> Term
(|-|) = notImplementedYet
(|*|) :: Term -> Term -> Term
(|*|) = notImplementedYet

-- Заменить переменную `varName` на `replacement`
-- во всём выражении `expression`
replaceVar :: String -> Term -> Term -> Term
replaceVar varName replacement expression =
   case expression of
      Variable { varName = varName' } | varName == varName' -> replacement
      BinaryTerm { op = op, lhv = lhv, rhv = rhv } ->
         BinaryTerm {
            op = op, 
            lhv = replaceVar varName expression lhv, 
            rhv = replaceVar varName expression rhv
         }
      _ -> expression

-- Посчитать значение выражения `Term`
-- если оно состоит только из констант
evaluate :: Term -> Term
evaluate = notImplementedYet
