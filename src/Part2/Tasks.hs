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
(|+|) = BinaryTerm Plus
(|-|) :: Term -> Term -> Term
(|-|) = BinaryTerm Minus
(|*|) :: Term -> Term -> Term
(|*|) = BinaryTerm Times

infixl 6 |+|
infixl 6 |-|
infixl 7 |*|

-- Заменить переменную `varName` на `replacement`
-- во всём выражении `expression`
replaceVar :: String -> Term -> Term -> Term
replaceVar varName replacement expression =
   case expression of
      Variable { varName = varName' } | varName == varName' -> replacement
      BinaryTerm { op = op, lhv = lhv, rhv = rhv } ->
         BinaryTerm {
            op = op, 
            lhv = replaceVar varName replacement lhv, 
            rhv = replaceVar varName replacement rhv
         }
      _ -> expression

-- Посчитать значение выражения `Term`
-- если оно состоит только из констант
evaluate :: Term -> Term
evaluate t =
   case t of
      BinaryTerm { op = op, lhv = lhv, rhv = rhv } ->
         evaluateOp (BinaryTerm { op = op, lhv = lhv', rhv = rhv' })
         where
            lhv' = evaluate lhv
            rhv' = evaluate rhv
      _ -> t
   where
      evaluateOp :: Term -> Term
      evaluateOp BinaryTerm {
         op = op,
         lhv = IntConstant { intValue = a },
         rhv = IntConstant { intValue = b }
      } = IntConstant $ evaluateOp' op a b
      evaluateOp t = t
      evaluateOp' Plus = (+)
      evaluateOp' Minus = (-)
      evaluateOp' Times = (*)
