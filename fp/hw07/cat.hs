import System.Environment
import System.IO
import System.IO.Error

main = do
    args <- getArgs
    let handles = case args of 
            [] -> [return (Right stdin)]
            names -> map (\n -> tryIOError $ openFile n ReadMode) names
        in cat handles
        where
            cat :: [IO (Either IOError Handle)] -> IO ()
            cat [] = return ()
            cat (x:xs) = do
                r <- x
                case r of
                    Left e -> putStrLn $ show e 
                    Right h -> cat' h 
                cat xs
            cat' h = do
                res <- tryIOError (hGetChar h)
                case res of
                    Left e -> if isEOFError e then return () else (ioError e)
                    Right c -> putChar c >> cat' h
