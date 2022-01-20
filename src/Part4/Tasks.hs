module Part4.Tasks where

import Util(notImplementedYet)
import GHC.List (init)

-- Перевёрнутый связный список -- хранит ссылку не на последующию, а на предыдущую ячейку
data ReverseList a = REmpty | (ReverseList a) :< a
infixl 5 :<

-- Функция-пример, делает из перевёрнутого списка обычный список
-- Использовать rlistToList в реализации классов запрещено =)
rlistToList :: ReverseList a -> [a]
rlistToList lst =
    reverse (reversed lst)
    where reversed REmpty = []
          reversed (init :< last) = last : reversed init

-- Реализуйте обратное преобразование
listToRlist :: [a] -> ReverseList a
listToRlist = reversed
    where reversed [] = REmpty
          reversed list = reversed (init list) :< last list

-- Реализуйте все представленные ниже классы (см. тесты)
instance (Show a) => Show (ReverseList a) where
    showsPrec = notImplementedYet
    show REmpty = "[]"
    show (init:<tail) =
        showl init ++ show tail ++ "]"
        where
            showl REmpty = "["
            showl (init:<tail) = showl init ++ show tail ++ ","
instance (Eq a) => Eq (ReverseList a) where
    (==) REmpty REmpty = True
    (==) REmpty _ = False
    (==) _ REmpty = False
    (==) (init1 :< last1) (init2 :< last2) = last1 == last2 && init1 == init2
    (/=) l1 l2 = not (l1 == l2)
instance Semigroup (ReverseList a) where
    (<>) l REmpty = l
    (<>) REmpty l = l
    (<>) l1 (init:<last) = (l1 <> init) :< last
instance Monoid (ReverseList a) where
    mempty = REmpty
instance Functor ReverseList where
    fmap func REmpty = REmpty
    fmap func (init:<last) = fmap func init :< func last
instance Applicative ReverseList where
  pure a = REmpty :< a
  (<*>) REmpty _ = REmpty
  (<*>) _ REmpty = REmpty
  (<*>) (fInit:<fLast) (lInit:<lLast) = (fInit <*> (lInit :< lLast)) <> (fmap fLast lInit :< fLast lLast)
instance Monad ReverseList where
  (>>=) REmpty func = REmpty
  (>>=) (init:<last) func = (init >>= func) <> func last
