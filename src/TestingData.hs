module TestingData where

import Terms

testTerm0 = Application
             (Abstraction "x" $ Application (Var "x") (Var "x"))
             (Abstraction "y" $ Var "y")


testTerm1 = Abstraction "x" (Application (Var "x") (Var "y"))
