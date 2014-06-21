-- (1.5 балла)
module Eval
    ( Eval, runEval
    , Error, Store
    , update, getVar
    ) where

import qualified Data.Map as M
import Data.List
import Control.Monad

import Expr

type Error = String
type Store = M.Map String Value

newtype Eval a = Eval { runEval :: Store -> (Maybe a, [Error], Store) }

instance Monad Eval where
    return x = Eval (\s -> (Just x, [], s))
    fail e = Eval (\s -> (Nothing, [e], s))
    Eval m >>= k = Eval bind'
        where
            bind' s = case m s of
                (Just x, e, s') -> let (y, e', s'') = runEval (k x) s' in (y, e ++ e', s'') 
                (_, e, s') -> (Nothing, e, s') 

-- MonadPlus - аналог Alternative для монад
-- mzero - вычисление, которое ничего не делает, сразу завершается неуспехом
-- mplus m1 m2 пытается выполнить m1, если тот завершился неуспехом, выполняет m2
-- Примеры использования этого класса есть в Utils.hs
instance MonadPlus Eval where
    mzero = Eval $ \s -> (Nothing, [], s) 
    mplus (Eval l) (Eval r) = Eval mplus'
        where
            mplus' s = case l s of
                (Nothing, e, s') -> let (x,  e', s'') = r s' in (x, e ++ e', s'') 
                x -> x  

update :: String -> Value -> Eval ()
update k v = Eval update'
    where
        update' s = (Just (), [], M.insert k v s) 

getVar :: String -> Eval Value
getVar v = Eval getVar'
    where
        getVar' s = maybe (Nothing, [v ++ " not found"], s) (\x -> (Just x, [], s)) $ M.lookup v s
