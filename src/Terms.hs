{-# LANGUAGE PatternSynonyms #-}

module Terms where
import Data.List (nub, (\\))
import Control.Applicative


{-
A. Data type to represent the untyped lambda calculous grammar:

t ::= x      -- Variable
    | λx.t   -- Abstraction
    | t t    -- Application
-}

type Symbol = String
data Term = Var Symbol
          | Abstraction Symbol Term 
          | Application Term Term

instance Show Term where
  show (Var s) = s
  show (Abstraction ins body) = "(λ" ++ ins ++ ". " ++ show body ++ ")"
  show (Application t1 t2) = "("++show t1 ++ " " ++ show t2++")"


-- B. Substitution function, with very primitive re-writing logic to avoid variable capture.
subst :: Symbol -> Term -> Term -> Term
subst x s (Var x') =  if x' == x then s else Var x'
subst x s (Abstraction y t1)
    | y /= x && y `notElem` freeVars s = Abstraction y $ subst x s t1       
    | otherwise = subst x s $ Abstraction y' (subst y (Var y') t1) -- rename y -> y' on the fly
    where y' = y ++ "'"
subst x s (Application t1 t2) = Application (recur t1) (recur t2)
  where recur = subst x s


freeVars :: Term -> [Symbol]
freeVars (Var x) = [x]
freeVars (Abstraction x t) = freeVars t \\ [x]
freeVars (Application t1 t2) = nub $ freeVars t1 ++ freeVars t2


-- C. Given a term, return true if term is a value.
isVal :: Term -> Bool
isVal (Abstraction _ _) = True
isVal _  = False



-- D. Implementation of single step reduction for call by value
eval1 :: Term -> Maybe Term
eval1 (Var _)              = Nothing 
eval1 (Abstraction s b)    = Abstraction s <$> eval1 b                 -- change to = nothing to implement call by name.
eval1 (Application t1 t2)  = case (t1,t2) of
      (Abstraction x b, t) -> pure $ subst x t b 
      _                    -> Application <$> eval1 t1 <*> pure t2    -- E-App1,  if the first term is a redux.
                          <|> Application <$> pure t1  <*> eval1 t2   -- E-App2,  if the second term is a redux.


whileJust :: (a -> Maybe a) -> a -> a

whileJust f a = case f a of
                  Just a' -> whileJust f a'
                  Nothing -> a

-- E. Eval, reduces terms to normal form.
eval :: Term -> Term
eval = whileJust eval1



evalPrinter :: Term -> IO ()
evalPrinter t = do
  print t
  case eval1 t of
    Just t' -> evalPrinter t'
    Nothing -> putStrLn "done"
  
  



  
              

-- Some common  terms
pattern X = Var "x"
pattern Y = Var "y"
pattern Z = Var "z"

t1 = Abstraction "x" (Application X X)
t2 = Abstraction "y" Y
demoTerm = Application t1 t2

testing :: IO ()
testing = do
  
  putStrLn $ show demoTerm ++ " ->* " ++ show (eval demoTerm)

  let t3 = (Application (Abstraction "x" (Application (Abstraction "y" Y) Z)) Y)
  putStrLn $ show t3 ++ " ->* " ++ show (eval t3)

  
