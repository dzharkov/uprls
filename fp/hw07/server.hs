import Network
import Control.Concurrent (forkIO)
import System.IO
import System.IO.Error
import System.Environment

server port = do
    s <- listenOn $ PortNumber $ (fromIntegral $ read port)
    serverLoop s 
    where
        serverLoop s = do
            (h,_,__) <- accept s
            forkIO (handleClient h)
            serverLoop s
        handleClient h = handleClient'
            where
                handleClient' = do
                    res <- tryIOError (hGetLine h)
                    case res of
                        Left e -> return () 
                        Right l -> (hPutStrLn h l) >> handleClient' 

main = do
    args <- getArgs
    let port = case args of {(a:xs) -> a}
        in server port
