import Data.List (nub, (\\))

type Symbol = String

data Term = Var Symbol
          | Abstraction { input :: Symbol
                        , body  :: Term
                        }
          | Application Term Term
          deriving(Eq)


instance Show Term where
  show (Var s) = s
  show (Abstraction ins body) = "(λ" ++ ins ++ "." ++ show body ++ ")"
  show (Application t1 t2) = show t1 ++ " " ++ show t2


freeVars :: Term -> [Symbol]
freeVars (Var x) = [x]
freeVars (Abstraction x t) = freeVars t \\ [x]
freeVars (Application t1 t2) = nub $ freeVars t1 ++ freeVars t2



subst :: Symbol -> Term -> Term -> Term
subst x (Var s1) (Var s2) = if s2 == x
                               then (Var s1)
                               else (Var s2)

subst x t1 (Abstraction i body) = Abstraction i (subst x t1 body)
subst x t1 (Application at1 at2) = Application at1' at2'
  where at1' = subst x t1 at1
        at2' = subst x t1 at2



testTerm0 = Application
             (Abstraction {input = "x", body = Application (Var "x") (Var "x")})
             (Abstraction {input = "y", body = Var "y"})


testTerm1 = Abstraction "x" (Application (Var "x") (Var "y"))
