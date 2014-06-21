-- (0.5 балла)
module Counter
    ( Counter
    , tick
    , runCounter
    ) where

-- Монада Counter считает количество тиков, т.е. вызовов функции tick
data Counter a = Counter Int a

-- Возвращает результат вычислений и количество тиков
runCounter :: Counter a -> (a, Int)
runCounter (Counter t x) = (x, t)  

instance Monad Counter where
    return = Counter 0 
    (Counter t x) >>= k = let (Counter t' y) = k x 
        in
        Counter (t' + t) y

tick :: Counter ()
tick = Counter 1 ()

main = do
    putStrLn "dsf"
    tick 1
    runCounter
