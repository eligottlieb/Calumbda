module UntypedLambda where

import Control.Monad.Except
import Control.Monad.Identity
import Control.Monad.Trans.Free
import PartialT
import Debug.Trace

data Command = StepTest Integer Term | ConfidenceTest Double Term deriving Show

data Value = IntConst Integer | Abstraction String Term (Maybe Env)
instance Show Value where
  show (IntConst n) = show n
  show (Abstraction str t _) = "λ" ++ str ++ "." ++ show t
data Term = Variable String | ValTerm Value | Application Term Term |
            Addition Term Term
instance Show Term where
  show (Variable str) = str
  show (ValTerm val) = show val
  show (Application func arg) = "(" ++ show func ++ " " ++ show arg ++ ")"
  show (Addition x y) = (case x of
    ValTerm (IntConst x') -> show x'
    _ -> "(" ++ show x ++ ")") ++ " + " ++ (case y of
    ValTerm (IntConst y') -> show y'
    _ -> "(" ++ show y ++ ")")

type Env = [(String, Value)]

defaultEnv :: Maybe Env -> Env
defaultEnv (Just e) = e
defaultEnv Nothing = []

data DynamicType = IntType | Arrow deriving Show

dynamicType :: Value -> DynamicType
dynamicType (IntConst _) = IntType
dynamicType (Abstraction _ _ _) = Arrow

data EvalError = UnboundVar String | DynamicTypeError DynamicType Value
  deriving Show
data StuckState = EvalFailure EvalError | UnfinishedTerm Term |
                  BadParse [String] deriving Show

getVar :: Env -> String -> ThrowsError StuckState Value
getVar [] var = throwError . EvalFailure $ UnboundVar var
getVar ((name, val) : env) var = if name == var
  then Right val
  else getVar env var

eval :: Env -> Term -> ThrowsPartial StuckState Value
eval env (Variable str) = liftPartial $ getVar env str
eval env (Application func arg) = rewrap $ do
  fval <- rewrap $ eval env func
  case fval of
    Abstraction var body closure -> do
          argVal <- rewrap $ eval env arg
          rewrap $ eval ((var, argVal):defaultEnv closure) body
    val@(_) -> throwError . EvalFailure $ DynamicTypeError Arrow val
eval env (Addition tx ty) = rewrap $ do
  x <- rewrap $ eval env tx
  y <- rewrap $ eval env ty
  case (x, y) of
    (IntConst x', IntConst y') -> return $ IntConst (x' + y')
    (IntConst _, _) -> throwError . EvalFailure $ DynamicTypeError IntType y
    _ -> throwError . EvalFailure $ DynamicTypeError IntType x
eval env (ValTerm (Abstraction var body Nothing)) = liftPartial . Right $
  Abstraction var body (Just env)
eval _ (ValTerm v) = liftPartial $ Right v
