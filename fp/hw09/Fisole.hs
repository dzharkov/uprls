-- (3 балла)

-- Список экспорта менять нельзя!
module Fisole
    ( Fisole, runFisole
    , abortF, putCharF, getCharF
    ) where

import System.IO.Error
import System.IO

data Fisole a = Fisole (Handle -> IO (Either IOError a))

instance Functor Fisole where
    fmap g f = do
        x <- f 
        return (g x)

instance Monad Fisole where
    return x = Fisole (\h -> return $ Right x)
    (Fisole m) >>= k = Fisole bind'
        where
            bind' h = do
                r <- m h
                case r of
                    (Left e) -> return $ Left e
                    (Right x) -> let (Fisole k') = k x in k' h

-- Второй аргумент - имя файла с которым будут работать функции putCharF и getCharF.
-- Если произошли какие-то ошибки ввода-вывода (т.е. исключения бросались), нужно их поймать и вернуть Left с каким-нибудь сообщением об ошибке.
runFisole :: Fisole a -> String -> IO (Either String a)
runFisole (Fisole f) s = do 
    hres <- tryIOError( openFile s ReadWriteMode )
    case hres of
        Left e -> return $ Left $ ioeGetErrorString e
        Right h -> do
            r <- f h
            hClose h
            case r of
                Left e -> return $ Left $ ioeGetErrorString e
                Right x -> return $ Right x

-- abortF s завершает вычисление, s - сообщение об ошибке.
abortF :: String -> Fisole a
abortF s = Fisole (\h -> return $ Left $ userError s) 

putCharF :: Char -> Fisole ()
putCharF c = Fisole $ putChar'
    where
        putChar' h = tryIOError (hPutChar h c)

getCharF :: Fisole Char
getCharF = Fisole $ getChar'
    where
        getChar' h = tryIOError (hGetChar h)
