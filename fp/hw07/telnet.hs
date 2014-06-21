import Network
import System.IO
import System.IO.Error
import System.Environment

telnet host port = do
    h <- connectTo host $ PortNumber $ (fromIntegral $ read port)
    readWrite h
    where
        readWrite h = readWrite'
            where
                readWrite' = do
                    isReady <- catchIOError (hReady h) (\e -> if isEOFError e then return False else ioError e) 
                    if isReady then readFromH >> readWrite' else do
                        isStdinEOF <- isEOF
                        if isStdinEOF then return () else do
                            l <- getLine 
                            hPutStrLn h l
                            readWrite'
                readFromH = do
                    c <- hGetChar h
                    putChar c
main = do
    args <- getArgs
    let (host, port) = case args of {(a:b:xs) -> (a, b)}
        in telnet host port
