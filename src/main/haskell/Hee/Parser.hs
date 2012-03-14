import Text.ParserCombinators.Parsec
import Hee.Terms

heeExpr :: Parser Term
heeExpr = heeQuote
      <|> heeCompose
      <|> heeEmpty

heeEmpty :: Parser Term
heeEmpty = eof >> return TmEmpty

heeQuote :: Parser Term
heeQuote = do char '['
              q <- heeExpr
              char ']'
              return $ TmQuote q

heeName :: Parser Term
heeName = do head <- noneOf "0123456789['\" \t\n"
             tail <- many (noneOf " \t\n]")
             return $ TmName (head:tail)

heeLiteral :: Parser Literal
heeLiteral = undefined
--      <|> heeString
--      <|> heeChar
--      <|> "0x" >> heeInt 16
--      <|> "0o" >> heeInt 8
--      <|> "0b" >> heeInt 2
--      <|> heeInt 10
--      <|> float

heeString :: Parser Literal
heeString = do char '"'
               cs <- many (noneOf "\"")
               char '"'
               return $ LiString cs

heeChar :: Parser Literal
heeChar = do char '\''
             c <- anyChar
             return $ LiChar c

heeInt :: Int -> Parser Int
heeInt radix = do digits <- many1 (oneOf $ take radix "0123456789abcdef")
                  return $ convert radix 0 digits
  where
    convert radix n []     = n
    convert radix n (d:ds) = convert radix (radix*n + digit d) ds
    digit '0' = 0
    digit '1' = 1
    digit '2' = 2
    digit '3' = 3
    digit '4' = 4
    digit '5' = 5
    digit '6' = 6
    digit '7' = 7
    digit '8' = 8
    digit '9' = 9
    digit 'a' = 10
    digit 'b' = 11
    digit 'c' = 12
    digit 'd' = 13
    digit 'e' = 14
    digit 'f' = 15

heeTerm :: Parser Term
heeTerm = heeName
-- <|> literal
   <|> heeQuote

heeCompose :: Parser Term
heeCompose = undefined

ws :: Parser ()
ws = skipMany space
