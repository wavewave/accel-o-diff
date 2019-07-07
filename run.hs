{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

-- automatic differentiation reformulated in terms of CPS transformation

module Main where

import Control.Error.Util (hoistMaybe,nothing)
import Control.Monad.Reader.Class (ask,local)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (MaybeT(runMaybeT))
import Control.Monad.Trans.Reader (Reader,runReader)
import Data.Bifunctor (first)
import Data.Hashable
import Data.List (lookup,nub)
import GHC.Generics (Generic)

data Val = Zero | One | Other Double
         deriving (Show, Generic)

instance Hashable Val

type Id = String

data Prim = Con Val
          | Var Id
          | Add Prim Prim
          | Mul Prim Prim
          deriving (Show, Generic)


instance Hashable Prim

data Exp = Prm Prim     -- ^ constant
         | Lam Id  Exp  -- ^ lambda
         | App Exp Exp  -- ^ function application
         deriving (Show, Generic)

instance Hashable Exp

add e1 e2 = App (App (Lam "x1" (Lam "x2" (Prm (Add (Var "x1") (Var "x2"))))) e1) e2
mul e1 e2 = App (App (Lam "y1" (Lam "y2" (Prm (Mul (Var "y1") (Var "y2"))))) e1) e2

-- data DVar = DVar Id

-- type DPrim = (Prim,DVar)

data DExp = Fwd Exp      -- forward  (non-lambda): ready for application
          | Bwd Id  DExp -- backward (lambda: (\y -> e(y)) dx): Id should be bound


class PrettyPrint a where
  pprint :: a -> String

instance PrettyPrint Val where
  pprint Zero      = "0"
  pprint One       = "1"
  pprint (Other v) = show v

instance PrettyPrint Prim where
  pprint (Con v) = pprint v
  pprint (Var x) = x
  pprint (Add p1 p2) = "(" ++ pprint p1 ++ "+" ++ pprint p2 ++ ")"
  pprint (Mul p1 p2) = "(" ++ pprint p1 ++ "*" ++ pprint p2 ++ ")"

instance PrettyPrint Exp where
  pprint (Prm p) = pprint p
  pprint (Lam x e) = "(\\" ++ x ++ " -> " ++ pprint e ++ ")"
  pprint (App e1 e2) = "(" ++ pprint e1 ++ " " ++ pprint e2 ++ ")"

-- instance PrettyPrint DVar where
--   pprint (DVar x) = "d" ++ x

-- instance PrettyPrint DPrim where
--   pprint (p,dp) = "(" ++ pprint p ++ ") " ++ pprint dp

-- instance PrettyPrint DExp where
--   pprint (e,dp) = "(" ++ pprint e ++ ") " ++ pprint dp

diffP :: Id -> Prim -> Prim
diffP _ (Con _)     = Con Zero
diffP x (Var y)     = if x == y then Con One else Con Zero
diffP x (Add p1 p2) = diffP x p1 `Add` diffP x p2
diffP x (Mul p1 p2) = (diffP x p1 `Mul` p2) `Add` (p1 `Mul` diffP x p2) -- Leibniz rule
--  map (first (\p -> Mul p p2)) (diffP x p1)
--                      ++ map (first (\p -> Mul p1 p)) (diffP x p2)

{-
toFwd f = uncurry Fwd . first f
-}

toLam :: Id -> DExp -> DExp
toLam y (F e)    = F (Lam y e) dx
toLam y (B z de) = B z (toLam y de)


toApp ex (F e)          = F (App e ex)
toApp ex (B y (F e))    = F (App (Lam y e) ex `mul` diff x ex)
toApp ex (B y (B z de)) =
  let e = undefined
  in B z (App (Lam y e) ex `mul` diff x ex



diff :: Id -> Exp -> [DExp]
diff x (Prm p)     = [Fwd (Prm (diffP x p))] -- (Prm (diffP x p), DVar x)
diff x (Lam y  e ) = map (Bwd y) (diff y e) ++ map (toLam y) (diff x e)
diff x (App e1 e2) =
  let de1's = diff x e1
  in concatMap (toApp e2) de1s

     case e1'                   in App e1' e2 `mul` diff x e2 -- chain rule


evalV :: Val -> Double
evalV Zero      = 0
evalV One       = 1
evalV (Other v) = v

x_ = Prm (Var "x")
y_ = Prm (Var "y")
z_ = Prm (Var "z")


t_exp1 = x_ `add` y_
-- t_dexp1 = diff "x" t_exp1

t_prim1 = Var "x" `Add` Var "y"
t_dprim1 = diffP "x" t_prim1
t_prim2 = Var "x" `Mul` Var "y"
t_dprim2 = diffP "y" t_prim2

main :: IO ()
main = do
  putStrLn "test"

  putStrLn (pprint t_prim1)
  mapM_ (putStrLn . pprint) t_dprim1

  putStrLn (pprint t_prim2)
  mapM_ (putStrLn . pprint) t_dprim2

{-
  print t_exp1
  putStrLn (pprint t_exp1)

  print t_dexp1
  putStrLn (pprint t_dexp1)
-}


{-
type VarMap = [(Id,Exp)]

type Eval = MaybeT (Reader VarMap)

runEval m = flip runReader m . runMaybeT

evalNaive :: Exp -> Eval Double
evalNaive (Con v)     = pure (evalV v)
evalNaive (Var x)     = ask >>= \m -> hoistMaybe (lookup x m) >>= \e -> evalNaive e
evalNaive (Add e1 e2) = (+) <$> evalNaive e1 <*> evalNaive e2
evalNaive (Mul e1 e2) = (*) <$> evalNaive e1 <*> evalNaive e2
evalNaive (Lam y  e ) = do m <- ask
                           e' <- hoistMaybe (lookup y m)
                           local ((y,e'):) $ evalNaive e
evalNaive (App l  e ) =
  case l of
    Lam x e' -> local ((x,e):) $ evalNaive e'
    _ -> nothing  -- for now

data Dep = Dep {
    depTgt :: Int
  , depSrc :: Int
  }

dep :: Exp -> [Dep]
dep (Con v) = []
dep (Var x) = []
dep e@(Add e1 e2) = map (Dep (hash e)) $ nub [hash e1, hash e2]
dep e@(Mul e1 e2) = map (Dep (hash e)) $ nub [hash e1, hash e2]
dep e@(Lam y  e') = [] -- [Dep (hash e) (hash e')]
dep e@(App l  e') = [Dep (hash e) (hash l), Dep (hash l) (hash e')]





addIfNew :: Exp -> [(Int,(Exp,[Dep]))] -> [(Int,(Exp,[Dep]))]
addIfNew e xs = let h = hash e
                in case lookup h xs of
                     Nothing -> (h,(e,dep e)) : xs
                     Just _ -> xs

callGraph :: Exp -> [(Int,(Exp,[Int]))] -> [(Int,(Exp,[Int]))]
callGraph e@(Con v)     xs = addIfNew e xs
callGraph e@(Var x)     xs = addIfNew e xs
callGraph e@(Add e1 e2) xs = addIfNew e (callGraph e2 (callGraph e1 xs))
callGraph e@(Mul e1 e2) xs = addIfNew e (callGraph e2 (callGraph e1 xs))
callGraph e@(Lam y  e') xs = addIfNew e (callGraph
callGraph e@(App l  e1) xs = addIfNew e (error "not yet defined")

-}



-- test = (x_ `Add` y_) `Mul` ((x_ `Add` x_) `Add` y_)

-- test = App (Lam "z" ((z_ `Mul` z_) `Mul` z_)) (x_ `Add` y_)



{-
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
-}

{-
  putStrLn "----------------"
  mapM_ print (callGraph test [])
  putStrLn "----------------"
  mapM_ print (callGraph (diff "x" test) [])
-}
