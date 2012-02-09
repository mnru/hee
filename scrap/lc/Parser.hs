module Parser
 ( Parser.parse
 ) where

import Text.ParserCombinators.Parsec
import Eval

parens p = between (symbol "(")
                   (symbol ")") p

whiteSpace = many $ oneOf " \t\n\r"

small = lower <|> char '_'
large = upper

idchar = small <|> large <|> digit <|> char '\''

lexeme p = do x <- p
              whiteSpace
              return x

symbol id = lexeme $ string id

ident :: CharParser () String
ident = lexeme $
        do c <- small
           cs <- many idchar
           return $ c : cs

var :: CharParser () Var
var = do string "$var:"
         v <- ident
         return $ AV v
  <|> do v <- ident
         return $ V v

exp :: CharParser () Exp
exp = do es <- many1 aexp
         return $ foldl1 App es

aexp :: CharParser () Exp
aexp = (try $ do v <- var
                 return $ Var v)
   <|> do symbol "\\"
          v <- var
          symbol "."
          e <- Parser.exp
          return $ Lam v e
   <|> parens Parser.exp
   <|> do string "$exp:"
          v <- ident
          return $ AE v

parse :: Monad m => String -> m Exp
parse s = case runParser p () "" s of
            Left e  -> fail $ show e
            Right e -> return e
  where p = do e <- Parser.exp
               eof
               return e
