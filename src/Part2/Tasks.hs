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
infixl 1 |+|
(|-|) :: Term -> Term -> Term
(|-|) = BinaryTerm Minus
infixl 1 |-|
(|*|) :: Term -> Term -> Term
(|*|) = BinaryTerm Times
infixl 2 |*|


-- Заменить переменную `varName` на `replacement`
-- во всём выражении `expression`
replaceVar :: String -> Term -> Term -> Term
replaceVar varName replacement expression = case expression of
    Variable currentName -> if currentName == varName then replacement else expression
    BinaryTerm op lhv rhv -> BinaryTerm op (replaceVar varName replacement lhv) (replaceVar varName replacement rhv)
    _ -> expression

-- Посчитать значение выражения `Term`
-- если оно состоит только из констант
evaluate :: Term -> Term
evaluate expr = case expr of
    BinaryTerm op lhv rhv ->
        case (op, evaluate lhv, evaluate rhv) of
            (Plus, IntConstant c1, IntConstant c2) -> IntConstant (c1 + c2)
            (Minus, IntConstant c1, IntConstant c2) -> IntConstant (c1 - c2)
            (Times, IntConstant c1, IntConstant c2) -> IntConstant (c1 * c2)
            _ -> expr
    _ -> expr
