module Part4.Tasks where

import Util(notImplementedYet)

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
listToRlist = listToRlist' . reverse
    where
        listToRlist' [] = REmpty
        listToRlist' (x:xs) = listToRlist' xs :< x 

-- Реализуйте все представленные ниже классы (см. тесты)
instance Show a => Show (ReverseList a) where
    show x = "[" ++ show' x ++ "]"
        where
            show' REmpty = ""
            show' (REmpty :< last) = show last
            show' (rest :< last) = show' rest ++ "," ++ show last
instance Eq a => Eq (ReverseList a) where
    (==) = eq
        where
            eq (r1 :< l1) (r2 :< l2)
                | l1 == l2 = eq r1 r2
                | otherwise = False
            eq REmpty REmpty = True
            eq _ _ = False
    (/=) a b = not (a == b)
instance Semigroup (ReverseList a) where
    (<>) = rconcat
        where
            rconcat rl REmpty = rl
            rconcat rl (rest :< last) = rconcat rl rest :< last
instance Monoid (ReverseList a) where
    mempty = REmpty
instance Functor ReverseList where
    fmap _ REmpty = REmpty
    fmap f (rest :< last) = fmap f rest :< f last
instance Applicative ReverseList where
    pure = (:<) REmpty
    (<*>) a b = concatMap (\f -> fmap f b) a
        where
            concatMap _ REmpty = REmpty
            concatMap f (rest :< last) = concatMap f rest <> f last

instance Monad ReverseList where
    (>>=) = bind
        where
            bind REmpty _ = REmpty
            bind (rest :< last) f = bind rest f <> f last
