module Terms
  ( Term (..)
  , subst
  ) where

import Data.List (nub, (\\))



type Symbol = String
data Term = Var Symbol
          | Abstraction { letS :: Symbol , body  :: Term }
          | Application Term Term
          deriving(Eq)



instance Show Term where
  show (Var s) = s
  show (Abstraction ins body) = "(λ" ++ ins ++ ". " ++ show body ++ ")"
  show (Application t1 t2) = show t1 ++ " " ++ show t2



freeVars :: Term -> [Symbol]
freeVars (Var x) = [x]
freeVars (Abstraction x t) = freeVars t \\ [x]
freeVars (Application t1 t2) = nub $ freeVars t1 ++ freeVars t2


isVal :: Term -> Bool
isVal (Abstraction _ _) = True
isVal _  = False


subst :: Symbol -> Term -> Term -> Term

subst x s (Var x') =  if x' == x then s else Var x'

subst x s (Abstraction y t1)
    | y /= x && y `notElem` freeVars s = Abstraction y $ subst x s t1       
    | otherwise = subst x s $ Abstraction (y ++ "'") (subst y (Var $ y ++ "'") t1)
                                   
subst x s (Application t1 t2) = Application (recur t1) (recur t2)
  where recur = subst x s



prettyPrintSubTest :: Symbol -> Term -> Term -> IO ()
prettyPrintSubTest s t1 t2 = do
    putStrLn $ "["++s++"->"++(show t1)++"]"++(show t2)++" => "++(show $ subst s t1 t2)    



mainz :: IO ()
mainz = do

  putStrLn "(pg 70)"
  let t1 = (Abstraction "z" (Application (Var "z") (Var "w")))
  let t2 = (Abstraction "y" (Var "x"))
  prettyPrintSubTest "x" t1 t2

  putStrLn("\n(pg 70)")
  prettyPrintSubTest "x" (Var "y") (Abstraction "x" (Var "x"))  
  
  

  
  let t1 = (Abstraction "z" (Application (Var "z") (Var "w")))
  let t2 = (Abstraction "y" (Var "x"))

  prettyPrintSubTest "x" t1 t2
    
  prettyPrintSubTest "x" (Var "z") (Abstraction "z" (Var "x"))
  
  prettyPrintSubTest "y" (Var "x") (Application (Var "y") (Var "z"))
  prettyPrintSubTest "y" (Var "x") (Var "y") 


  prettyPrintSubTest "y" (Var "z") (Abstraction "x" (Application (Var "x") (Var "y")))
  -- let ta = (Application (Var "y") (Var "z"))
  -- let tb = Abstraction "y" (Application (Var "x") (Var "y"))
  -- putStrLn $ show $ subst "x" ta tb

  
  let t1 = (Application (Var "f") (Var "y"))
  let t2 = (Abstraction "y"
            (Application
             (Abstraction "f" (Application (Var "f") (Var "x")))
             (Var "y")
            ))
           
  putStrLn $ show t1
  putStrLn $ show t2
  prettyPrintSubTest "x" t1 t2
