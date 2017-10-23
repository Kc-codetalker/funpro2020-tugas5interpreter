module Terms
  ( Term (..)
  , subst
  , eval
  ) where

import Data.List (nub, (\\))
import Control.Applicative


type Symbol = String
data Term = Var Symbol
          | Abstraction Symbol Term 
          | Application Term Term
          deriving(Eq)



instance Show Term where
  show (Var s) = s
  show (Abstraction ins body) = "(λ" ++ ins ++ ". " ++ show body ++ ")"
  show (Application t1 t2) = "("++show t1 ++ " " ++ show t2++")"




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
    | otherwise = subst x s $ Abstraction y' (subst y (Var y') t1)
    where y' = y ++ "'"
subst x s (Application t1 t2) = Application (recur t1) (recur t2)
  where recur = subst x s


eval1 :: Term -> Maybe Term
eval1 (Var _) = Nothing
eval1 (Abstraction s b) = Nothing
eval1 (Application t1 t2) = case (t1,t2) of
          (Abstraction x b, t) -> pure $ subst x t b 
          _ -> (Application <$> eval1 t1 <*> pure t2)   -- E-App1, 
            <|>  (Application <$> pure t1 <*> eval1 t2) -- E-App2,



evaluator :: (Term -> Maybe Term) -> Term -> Term
evaluator step t = case step t of
                    Just t' -> evaluator step t'
                    Nothing -> t


callbyValue :: Term -> Maybe Term
callbyValue (Abstraction s b) = (Abstraction s) <$> callbyValue b
callbyValue t = eval1 t


eval :: Term -> Term
eval = evaluator eval1

----- testing

--t1 = (Application (Abstraction "x" (Application (Var "x") (Var "x"))) (Abstraction "y" (Var "y")))
-- x = Var "x"
-- y = Var "y"
-- z = Var "z"

  
c_0 = (Abstraction "s" (Abstraction "z" (Var "z")))
succ' = (Abstraction "n"
         (Abstraction "s"
          (Abstraction "z"
           (Application
            (Var "s")
            (Application (Application (Var "n") (Var "s")) (Var "z"))))))


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
