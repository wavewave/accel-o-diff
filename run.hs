{-# LANGUAGE DeriveGeneric #-}
module Main where

import Control.Error.Util (hoistMaybe)
import Control.Monad.Reader.Class (ask,local)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (MaybeT,runMaybeT)
import Control.Monad.Trans.Reader (Reader,runReader)
import Data.Hashable
import Data.List (lookup,nub)
import GHC.Generics (Generic)

data Val = Zero | One | Other Double
         deriving (Show, Generic)

instance Hashable Val

data Lam = Lam String Exp -- variable (local), expresseion
           deriving (Show, Generic)

instance Hashable Lam

data Exp = Con Val      -- ^ constant
         | Var String   -- ^ variable
         | Add Exp Exp  -- ^ add
         | Mul Exp Exp  -- ^ multiply
         | App Lam Exp  -- ^ lambda application
         deriving (Show, Generic)

instance Hashable Exp


diff :: String -> Exp -> Exp
diff _ (Con _)     = Con Zero
diff x (Var y)     = if x == y then Con One else Con Zero
diff x (Add e1 e2) = Add (diff x e1) (diff x e2)
diff x (Mul e1 e2) = Mul (diff x e1) e2 `Add` Mul e1 (diff x e2)
diff x (App l  e1) = case l of
                       Lam y e2 -> let l' = Lam y (diff y e2)
                                   in App l' e1 `Mul` diff x e1 -- chain rule

evalV :: Val -> Double
evalV Zero      = 0
evalV One       = 1
evalV (Other v) = v

type Identifier = String

type VarMap = [(Identifier,Exp)]

type Eval = MaybeT (Reader VarMap)

runEval m = flip runReader m . runMaybeT

evalNaive :: Exp -> Eval Double
evalNaive (Con v)     = pure (evalV v)
evalNaive (Var x)     = ask >>= \m -> hoistMaybe (lookup x m) >>= \e -> evalNaive e
evalNaive (Add e1 e2) = (+) <$> evalNaive e1 <*> evalNaive e2
evalNaive (Mul e1 e2) = (*) <$> evalNaive e1 <*> evalNaive e2
evalNaive (App l  e1) =
  case l of
    Lam x e2 -> local ((x,e1):) $ evalNaive e2


dep :: Exp -> [Int]
dep (Con v) = []
dep (Var x) = []
dep (Add e1 e2) = nub [hash e1, hash e2]
dep (Mul e1 e2) = nub [hash e1, hash e2]
dep (App (Lam x e1) e2) = [hash e2] -- this is unclear

addIfNew :: Exp -> [(Int,(Exp,[Int]))] -> [(Int,(Exp,[Int]))]
addIfNew e xs = let h = hash e
                in case lookup h xs of
                     Nothing -> (h,(e,dep e)) : xs
                     Just _ -> xs

callGraph :: Exp -> [(Int,(Exp,[Int]))] -> [(Int,(Exp,[Int]))]
callGraph e@(Con v)     xs = addIfNew e xs
callGraph e@(Var x)     xs = addIfNew e xs
callGraph e@(Add e1 e2) xs = addIfNew e (callGraph e2 (callGraph e1 xs))
callGraph e@(Mul e1 e2) xs = addIfNew e (callGraph e2 (callGraph e1 xs))
callGraph e@(App l  e1) xs = addIfNew e (error "not yet defined")


x_ = Var "x"
y_ = Var "y"
z_ = Var "z"

-- test = (x_ `Add` y_) `Mul` ((x_ `Add` x_) `Add` y_)

test = App (Lam "z" ((z_ `Mul` z_) `Mul` z_)) (x_ `Add` y_)



main :: IO ()
main = do
  putStrLn "testing"
  print test
  print (diff "x" test)
  print (diff "y" test)

  let m = [("x",Con One),("y",Con One)]
  print (runEval m (evalNaive test))
  print (runEval m (evalNaive (diff "x" test)))
  print (runEval m (evalNaive (diff "y" test)))

  putStrLn "----------------"
  mapM_ print (callGraph test [])
  putStrLn "----------------"
  mapM_ print (callGraph (diff "x" test) [])
