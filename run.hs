module Main where

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

eval :: [(String,Exp)] -> Exp -> Maybe Double
eval _ (Con v)     = Just $ evalV v
eval m (Var x)     = lookup x m >>= \e -> eval m e
eval m (Add e1 e2) = (+) <$> eval m e1 <*> eval m e2
eval m (Mul e1 e2) = (*) <$> eval m e1 <*> eval m e2

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
  print (eval m test)
  print (eval m (diff "x" test))
  print (eval m (diff "y" test))
