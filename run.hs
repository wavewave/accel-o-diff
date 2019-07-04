module Main where

import Control.Error.Util (hoistMaybe)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (MaybeT,runMaybeT)
import Control.Monad.Trans.Reader (Reader,ask,runReader)
import Data.List (lookup)

data Val = Zero | One
         deriving Show

data Exp = Con Val      -- ^ constant
         | Var String   -- ^ variable
         | Add Exp Exp  -- ^ add
         | Mul Exp Exp  -- ^ multiply
         deriving Show

diff :: String -> Exp -> Exp
diff _ (Con _)     = Con Zero
diff x (Var y)     = if x == y then Con One else Con Zero
diff x (Add e1 e2) = Add (diff x e1) (diff x e2)
diff x (Mul e1 e2) = Mul (diff x e1) e2 `Add` Mul e1 (diff x e2)

evalV :: Val -> Double
evalV Zero = 0
evalV One  = 1

type VarMap = [(String,Exp)]

eval :: Exp -> MaybeT (Reader VarMap) Double
eval (Con v)     = pure (evalV v)
eval (Var x)     = lift ask >>= \m -> hoistMaybe (lookup x m) >>= \e -> eval e
eval (Add e1 e2) = (+) <$> eval e1 <*> eval e2
eval (Mul e1 e2) = (*) <$> eval e1 <*> eval e2

x_ = Var "x"
y_ = Var "y"

test = (x_ `Add` y_) `Mul` ((x_ `Add` x_) `Add` y_)



main :: IO ()
main = do
  putStrLn "testing"
  print test
  print (diff "x" test)
  print (diff "y" test)

  let m = [("x",Con One),("y",Con One)]
  print (runReader (runMaybeT (eval test)) m)
  print (runReader (runMaybeT (eval (diff "x" test))) m)
  print (runReader (runMaybeT (eval (diff "y" test))) m)
