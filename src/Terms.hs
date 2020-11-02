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
          | Digit Int

instance Show Term where
  show (Var s) = s
  show (Abstraction ins body) = "(λ" ++ ins ++ ". " ++ show body ++ ")"
  show (Application t1 t2) = "("++show t1 ++ " " ++ show t2++")"
  show (Digit n) = show n


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
eval term = termToDigit (whileJust eval1 term)


-- F. Tambahkan 1 layer ke Eval untuk cek kalau bentuknya number jadiin digit
-- Kalau snd dari (Num, Bool) nilainya False, dia bukan number dan langsung keluarin Term asli
-- Kalau snd dari (Num, Bool) nilainya True, dia number, ambil fst-nya angka brp
termToDigit :: Term -> Term
termToDigit term = if (snd recurse) then Digit (fst recurse)
                                    else term
                                    where recurse = firstLvlTermToDigit term

-- lalaLvlTermToDigit akan kembalikan (Num, Bool), Num ini jumlah variabel berurutan yg untuk hitung bilangan church numeral.
-- Bool kalau True berarti term full ekspresi digit, kalo False berarti term bukan ekspresi digit.
-- firstLvlTermToDigit (λx. (λx'. (x (x (x (x (x (x x')))))))) akan diekspek return (6, True) yg berarti dia angka 6.
-- Term pertama dari Church Numeral haruslah Abstraction.
firstLvlTermToDigit :: Num t => Term -> (t, Bool)
firstLvlTermToDigit (Abstraction ins body) = sndLvlTermToDigit ins body
firstLvlTermToDigit _ = (-1, False)

-- Dalam Church's Numeral, setelah lambda pertama harus Abstraction lagi variabel kedua
sndLvlTermToDigit :: Num t => Symbol -> Term -> (t, Bool)
sndLvlTermToDigit var1 (Abstraction ins body) = bodyLvlTermToDigit var1 ins body
sndLvlTermToDigit var1 _ = (-1, False)

-- Setelah ada dua nested lambda, harus dilanjutkan Application
-- Bentuk Church's Numeral adalah \var1.\var2.(var1(var1...(var1 var2)))
bodyLvlTermToDigit :: Num t => Symbol -> Symbol -> Term -> (t, Bool)
bodyLvlTermToDigit var1 var2 (Application (Var s1) t2) = if s1 == var1
                                                         then (1 + (fst recurse), True && (snd recurse))
                                                         else (-1, False)
                                                         where recurse = bodyLvlTermToDigit var1 var2 t2
bodyLvlTermToDigit var1 var2 (Var s) = if s == var2
                                       then (0, True) -- base case sudah sampai (var1 var2)
                                       else (-1, False)
bodyLvlTermToDigit var1 var2 _ = (-1, False) -- sudah sampai bentuk terdalam ternyata bukan aplikasi atau variabel


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


