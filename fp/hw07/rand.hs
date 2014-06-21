import System.Random

guess :: Int -> IO ()
guess n = guess' 5
    where
        guess' 0 = putStrLn "looser"
        guess' a = do
            putStrLn "try"
            z <- readLn 
            result z
                where
                    result z | z < n = putStrLn "your number is less" >> (guess' $ pred a)
                    result z | z > n = putStrLn "your number is greater" >> (guess' $ pred a)
                    result z | z == n = putStrLn "win" 

main = do
    n <- randomRIO (1,100)
    guess n
