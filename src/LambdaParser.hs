module LambdaParser where


import Text.ParserCombinators.Parsec
import Terms


stripWs :: Parser a -> Parser a
stripWs p = spaces *> p


parseLambdaSymbol :: Parser String
parseLambdaSymbol = string "\\"

varParser :: Parser Term
varParser = Var <$> many1 letter

token' :: String -> Parser String
token' sym = stripWs $  string sym

parenthesised :: Parser a -> Parser a
parenthesised p = token' "(" *> p <* token' ")"


absParser :: Parser Term
absParser = Abstraction <$> binding <*> expParser
  where inputP = many1 letter
        dot = char '.'
        binding = parseLambdaSymbol *> inputP <* dot

appParser :: Parser Term
appParser = do
  t1 <- nonAppParser <|> expParser
  spaces
  t2 <- nonAppParser <|> expParser
  return $ Application t1 t2

nonAppParser :: Parser Term
nonAppParser = (varParser <|> absParser)


expParser :: Parser Term
expParser = (try (nonAppParser <* spaces <* eof)) <|> appParser
          
  






