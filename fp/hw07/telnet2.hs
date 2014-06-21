import Network
import System.IO
import System.IO.Error
import System.Environment
import Control.Concurrent.Async

telnet host port = do
    h <- connectTo host $ PortNumber $ (fromIntegral $ read port)
    start h
    where
        start h = start'
            where 
                start' = do
                    f1 <- async readFromServer
                    f2 <- async readFromClient
                    wait f1
                    return ()
                readFromServer = do
                    res <- tryIOError ( hGetChar h )
                    case res of
                        Left e -> return ()
                        Right c -> (putChar c >> readFromServer)
                readFromClient = do
                    res <- tryIOError ( getLine )
                    case res of
                        Left e -> return ()
                        Right l -> (hPutStrLn h l >> readFromClient)
                    
                
main = do
    args <- getArgs
    let (host:port:xs) = args in telnet host port
