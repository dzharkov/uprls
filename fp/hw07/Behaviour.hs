-- Реализуйте runBehaviour
-- (1 балл)

data Request  = Get           | Put String
data Response = Result String | OK

type Behaviour = [Response] -> [Request]

prog :: Behaviour
prog ~(OK : x : xs) = Put "more? " : Get : case x of
    Result "no" -> []
    Result "yes" -> prog xs
    _ -> Put "yes or no!" : prog (tail xs)

main = runBehaviour prog

runBehaviour :: Behaviour -> IO ()
runBehaviour p = go [] 
    where
        drop' :: [a] -> [b] -> [b]
        drop' [] y = y
        drop' (x:xs) (y:ys) = drop' xs ys

        go :: [Response] -> IO ()
        go r1 = case drop' r1 $ p r1 of
            [] -> return ()
            ((Put x):xs) -> putStrLn x >> (go $ r1++[OK]) 
            ((Get):xs) -> getLine >>= (\v -> go $ r1++[Result v])
