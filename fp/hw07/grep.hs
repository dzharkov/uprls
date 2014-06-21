import System.Environment
import System.IO
import System.IO.Error
import Data.List

main = do
    args <- getArgs
    let (s, handles) = case args of 
            (x:names) -> (x, map (\n -> tryIOError $ openFile n ReadMode) names)
        in grep s handles
    where
        grep :: String -> [IO (Either IOError Handle)] -> IO ()
        grep s [] = return ()
        grep s (x:xs) = do
            r <- x
            case r of
                Left e -> putStrLn $ show e 
                Right h -> grep' s h 
            grep s xs
        grep' s h = grep''
            where 
                grep'' = do 
                    res <- tryIOError (hGetLine h)
                    case res of
                        Left e -> if isEOFError e then return () else (ioError e)
                        Right l -> (if isInfixOf s l then putStrLn l else return ()) >> grep''  
