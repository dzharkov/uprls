-- (0.5 балла)
import Counter

-- Эти две функции отличаются от обычных тем, что, вызывая tick, они считают сколько шагов заняло их выполнение.
filter' :: (a -> Bool) -> [a] -> Counter [a]
filter' p xs = filter'' xs 
    where
        filter'' [] = return []
        filter'' (x:xs) = do
            tick
            ys <- filter'' xs
            return $ if p x then x:ys else ys

append :: [a] -> [a] -> Counter [a]
append a b = append' a 
    where
        append' [] = return b
        append' (x:xs) = do
            tick
            ys <- append' xs
            return $ x:ys

-- Реализуйте qsort через filter' и append
qsort :: Ord a => [a] -> Counter [a]
qsort [] = return []
qsort (x:xs) = do
    l <- (filter' (<= x) xs) >>= qsort 
    r <- (filter' (> x) xs) >>= qsort >>= (\q -> return $ x:q)
    append l r

-- Первый вызов должен занимать большее количество тиков ~ в 2 раза
main = do
    print $ runCounter $ qsort [1..15]
    print $ runCounter $ qsort [8,4,12,2,6,10,14,1,3,5,7,9,11,13,15]
