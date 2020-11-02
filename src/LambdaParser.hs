{-
Code adapted from  David Ringo : https://github.com/dmringo
-}
module LambdaParser (parseLambda) where

import Text.ParserCombinators.ReadP
import Data.Char (isAlpha, isDigit, isPunctuation, digitToInt)
import Control.Arrow ((>>>))
import Terms


parseLambda :: String -> Maybe Term
parseLambda s = runparse term (replaceArithmeticsToChurches s)

replaceArithmeticsToChurches s = replaceMultToChurch (replacePlusToChurch (replaceNumbersToChurches s (filter isDigit s)))

-- menerima string input dan list karakter digit, replace setiap number dengan bentuk churchnya
replaceNumbersToChurches :: String -> [Char] -> String
replaceNumbersToChurches str [] = str
replaceNumbersToChurches str (first:rest) = replaceNumbersToChurches (replaceNumberToChurch str first) rest

-- replace satu jenis number dengan bentuk churchnya
replaceNumberToChurch :: String -> Char -> String
replaceNumberToChurch xs num = replaceCharWithString xs num (numberToChurch (digitToInt num))

-- replace semua tanda '+' dengan bentuk churchnya
replacePlusToChurch :: String -> String
replacePlusToChurch xs = replaceCharWithString xs '+' plus

-- replace semua tanda '*' dengan bentuk churchnya
replaceMultToChurch :: String -> String
replaceMultToChurch xs = replaceCharWithString xs '*' mult

-- https://www.manongdao.com/q-1091905.html replace implementation, edited with some fixes
replaceCharWithString :: String -> Char -> String -> String
replaceCharWithString xs char str = foldr go [] xs
 where go x acc = if x == char then str ++ acc
                               else x:acc

numberToChurch :: Int -> String
numberToChurch 0 = "(\\x.\\y.y)"
numberToChurch x = "(" ++ plus ++ (numberToChurch (x-1)) ++ ")"

plus :: String
plus = arithmeticToChurch '+'

mult :: String
mult = arithmeticToChurch '*'

arithmeticToChurch :: Char -> String
arithmeticToChurch '+' = "(\\w.\\y.\\x.(y (w y x)))"
arithmeticToChurch '*' = "(\\w.\\y.\\x.(w (y x)))"


runparse :: ReadP a -> String -> Maybe a
runparse p s = case readP_to_S (p <* skipSpaces <* eof) s of
  [(v,"")] -> Just v
  _        -> Nothing


stripWS :: ReadP a -> ReadP a
stripWS = (skipSpaces *>)


token :: String -> ReadP String
token = stripWS . string


lambda, dot, varname :: ReadP String
lambda = token "\\"
dot = token "."
varname = (:) <$> stripWS (satisfy isAlpha) <*> munch ((&&) <$> isAlpha <*> (not . isPunctuation))


parens :: ReadP a -> ReadP a
parens =  (token "(" *>) >>> (<* token ")")



varP, absP, appP, term :: ReadP Term
term = absP <++ appP <++ varP <++ parens term
varP = Var <$> varname
absP = Abstraction <$> (lambda *> varname <* dot) <*> term
appP = chainl1 nonApp (skipSpaces *> pure Application)
  where nonApp = absP +++ varP +++ parens term



