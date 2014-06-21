{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}

import qualified Data.Map as M
import Data.List
import Control.Monad.Trans.State
import Control.Monad.State.Class
import Control.Monad.Error.Class 
import qualified Control.Monad.Trans.Error as TE
import Control.Monad.Trans
import Control.Monad.Identity
import Control.Lens
import Data.Bifunctor

data Term = Var String | Term :@ Term | Lambda String Term deriving (Eq)
data Type = TVar String | Type :-> Type deriving (Eq)

instance Show Term where
    show (Var x) = x
    show (l@(Var x) :@ r@(Lambda x' t)) = (show l) ++ "(" ++ (show r) ++ ")"
    show (l@(Var x) :@ r@(Var y)) = (show l) ++ (show r)
    show (l@(Var x) :@ r) = (show l) ++ "(" ++ (show r) ++ ")"
    show (l@(Lambda x y) :@ z) = "(" ++ (show l) ++ ")" ++ (show z)
    show (l :@ r@(Var y)) = (show l) ++ (show r)
    show (l :@ z) = "(" ++ (show l) ++ ")" ++ "(" ++ (show z) ++ ")"
    show l@(Lambda x t) = "λ" ++ (showLambda l)
        where 
            showLambda (Lambda x r@(Lambda y t)) = x ++ (showLambda r)
            showLambda (Lambda x b) = x ++ "." ++ (show b)
    

instance Show Type where
    show (TVar x) = x
    show (TVar x :-> y) = x ++ " -> " ++ (show y) 
    show (x :-> y) = "(" ++ (show x)  ++ ")" ++ " -> " ++ (show y) 

type TypeContext = M.Map String Type

infixl 2 :@
infixr 2 :->

type Equation = (Type, Type)
type SubstitutionPart = (String, Type)

data Store = Store { _freshVariableCounter :: Int, 
                     _equations :: [Equation],
                     _typeVarMap :: [SubstitutionPart]
                   } deriving Show 

$(makeLenses ''Store)

newtype TypeInference a = TypeInference { runSolver :: TE.ErrorT String (StateT Store IO) a 
                          } deriving (Monad, MonadState Store, MonadPlus, MonadError String, MonadIO)

freshVariable :: (MonadState Store m) => m Type
freshVariable = do
    r <- use freshVariableCounter 
    freshVariableCounter %= succ 
    return $ TVar $ "a" ++ (show r) 

appendEquation :: Equation -> TypeInference ()
appendEquation e = do
    equations %= (e:)
    return ()

makeEquations :: TypeContext -> Term -> TypeInference Type
makeEquations c = makeEquations'
    where
        makeEquations' :: Term -> TypeInference Type
        makeEquations' (Var x) = maybe (throwError $ "undefined var " ++ x) return $ M.lookup x c
        makeEquations' (x :@ y) = do
            xt <- makeEquations' x 
            yt <- makeEquations' y 
            r <- freshVariable
            appendEquation (xt, yt :-> r)
            return r 
        
        makeEquations' (Lambda x t) = do
            freshVar <- freshVariable
            tType <- makeEquations (M.insert x freshVar c) t
            return (freshVar :-> tType)

freeVars :: Type -> [String]
freeVars (TVar x) = [x]
freeVars (x :-> y) = (freeVars x) ++ (freeVars y)

applySubstitutionPart :: SubstitutionPart -> Type -> Type
applySubstitutionPart (name, s) t = applySubstitutionPart' t
    where
        applySubstitutionPart' (TVar y) | name == y = s
        applySubstitutionPart' a@(TVar y) | otherwise = a
        applySubstitutionPart' (a :-> b) = (applySubstitutionPart' a) :-> (applySubstitutionPart' b)

untilM_ :: Monad m => m Bool -> m a -> m ()
untilM_ f b = do
    r <- f
    if not r then do
        b
        untilM_ f b
    else return ()

applyTypeVarMap :: Type -> TypeInference Type
applyTypeVarMap t = use typeVarMap >>= (return . (foldl (flip applySubstitutionPart) t) . reverse)


findUnifier :: TypeInference () 
findUnifier = do
    typeVarMap .= []
    untilM_ (use equations >>= return . null) $ do
        (x:xs) <- use equations
        equations .= xs
        handleEquation x
    where
        handleEquation :: Equation -> TypeInference ()
        handleEquation (x1 :-> x2, y1 :-> y2) = do
            handleEquation (x1, y1)
            x2' <- applyTypeVarMap x2
            y2' <- applyTypeVarMap y2
            handleEquation (x2', y2')
        handleEquation (TVar x, TVar y) | x == y = return ()
        handleEquation (TVar x, y) | elem x $ freeVars y = throwError "infinite type inference"
        handleEquation (TVar x, y) | otherwise = do 
            let part = (x,y)
            typeVarMap %= (part:)
            equations %= map (join bimap $ applySubstitutionPart part)
            return ()
        handleEquation (l@(x :-> y), r) = handleEquation (r, l)

infereType :: TypeContext -> Term -> TypeInference Type
infereType context term = do
    equations .= []
    redundantType <- makeEquations context term
    findUnifier
    applyTypeVarMap redundantType

runTypeInference :: TypeInference a -> IO (Either String a)
runTypeInference x = evalStateT (TE.runErrorT $ runSolver x) (Store 0 [] [])

terms = 
    [
        ([], (Lambda "x" $ Lambda "y" $ Var "x" :@ Var "y"))
        ,([], (Lambda "x" $ Lambda "y" $ Var "y" :@ Var "x"))
        ,([], (Lambda "x" $ Var "x") :@ (Lambda "x" $ Var "x"))
        ,([], (Lambda "x" $ Lambda "x" $ Var "x"))
        ,([], (Lambda "x" $ Lambda "y" $ Var "x" :@ (Var "y" :@ Var "x")))
        ,([], (Lambda "f" $ Lambda "g" $ Lambda "x" $ (Var "f" :@ Var "x") :@ (Var "g" :@ Var "x")))
        ,([], (Lambda "x" $ Lambda "y" $ Var "y" :@ (Lambda "z" $ Var "y" :@ Var "x")))
        ,([("x", TVar "sigma")], (Lambda "y" $ Var "x"))
        ,([], (Lambda "y" $ Var "x"))
        ,([("x", TVar "sigma")], Var "x" :@ Var "x")
    ]

main = forM_ terms (uncurry processTerm)  
    where
        processTerm context term = do
            putStr $ intercalate ", " $ map (\(x, t) -> x ++ "=" ++ (show t)) context           
            putStr " ⊢ "
            putStr $ (show term) ++ " : "
            (runTypeInference $ infereType (M.fromList context) term) >>= (either putStrLn (putStrLn . show))
