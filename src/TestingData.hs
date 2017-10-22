module TestingData where

import Terms

testTerm0 = Application
             (Abstraction {letS  = "x", body = Application (Var "x") (Var "x")})
             (Abstraction {letS  = "y", body = Var "y"})


testTerm1 = Abstraction "x" (Application (Var "x") (Var "y"))
