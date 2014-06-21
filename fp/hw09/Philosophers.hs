-- (4 балла)

import System.Random
import Control.Monad
import Control.Concurrent.STM
import Control.Concurrent
import System.Environment
{-
Решите задачу о философах http://en.wikipedia.org/wiki/Dining_philosophers_problem
Количество философов передается в параметрах командной строки.
Жизненый цикл философа:
a) Философ сообщает (вывод сообщения на экран) о том, что он готов обедать.
b) Ждет пока не освободятся обе вилки.
c) Берет вилки, сообщает об этом, начинает обедать, что занимает рандомное время (от 1 до 3 секунд).
d) Кладет вилки, сообщает об этом, начинает думать, что занимает рандомное время (от 1 до 3 секунд).
e) Возвращается к шагу (a).

Для реализации используйте библиотеку STM.
Вам также понадобятся функции forkIO, threadDelay и randomRIO.
-}

data Fork = Fork { fid :: Int, capturedBy :: TVar Int }
data Philosopher = Philosopher { pid :: Int, leftFork :: Fork, rightFork :: Fork }

instance Eq Fork where
    (Fork x _) == (Fork y _) = x == y

instance Ord Fork where
    (Fork x _) <= (Fork y _) = x <= y

nobodyCaptured :: Int
nobodyCaptured = -1

makeForks :: Int -> IO [Fork]
makeForks n = sequence $ map newFork [0..n-1] 
    where
        newFork i = atomically $ do
            r <- newTVar (-1)
            return $ Fork i r

makePhilosophers :: Int -> IO [Philosopher]
makePhilosophers n = do
    forks <- makeForks n
    return $ make' 0 $ cycle forks

    where
        make' i _ | i == n = []   
        make' i (x:y:xs) = (Philosopher i y x):(make' (i+1) (y:xs))  

firstForkVar :: Philosopher -> TVar Int
firstForkVar p = capturedBy $ min (rightFork p) (leftFork p) 

secondForkVar :: Philosopher -> TVar Int
secondForkVar p = capturedBy $ max (rightFork p) (leftFork p)

runPhilosopher :: Philosopher -> IO ()
runPhilosopher p = runPhilosopher'
    where
        runPhilosopher' = do
            putStrLn ((show $ pid p) ++ " is ready")
            atomically acquireForks
            
            putStrLn ((show $ pid p) ++ " got forks")
            waitRandom
            
            putStrLn ((show $ pid p) ++ " free forks")
            atomically putForks
            waitRandom
            
            runPhilosopher'
        waitRandom = do
            t <- randomRIO (1000000, 3000000)
            threadDelay t
        putForks = do
            writeTVar (capturedBy $ leftFork p) nobodyCaptured
            writeTVar (capturedBy $ rightFork p) nobodyCaptured
        acquireForks = do
            l <- readTVar $ firstForkVar p
            let currentPid = pid p
            if (l /= nobodyCaptured) && (l /= currentPid) then retry else do
                writeTVar (firstForkVar p) currentPid
                r <- readTVar $ secondForkVar p
                if (r /= nobodyCaptured) && (r /= currentPid) then retry else do
                    writeTVar (secondForkVar p) currentPid

main = do
    (n:_) <- getArgs
    ps <- makePhilosophers ((read n)::Int)
    forkAll ps
    where
        forkAll (p:[]) = runPhilosopher p
        forkAll (p:xs) = do
            forkIO (runPhilosopher p)
            forkAll xs
