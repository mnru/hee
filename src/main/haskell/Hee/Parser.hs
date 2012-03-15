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
heeLiteral = heeString
         <|> heeChar
         <|> try (string "0x" >> heeInt "0x")
         <|> try (string "0o" >> heeInt "0o")
         <|> try (string "0b" >> heeInt "0b")
         <|> try heeFloat
         <|> heeInt ""

heeString :: Parser Literal
heeString = do char '"'
               cs <- many (noneOf "\"")
               char '"'
               return $ LiString cs

heeChar :: Parser Literal
heeChar = do char '\''
             c <- anyChar
             return $ LiChar c

heeInt :: String -> Parser Literal
heeInt radix = do digits <- many1 (oneOf "0123456789abcdef")
                  let parsed = read (radix ++ digits)
                  return $ LiInt parsed

heeFloat :: Parser Literal
heeFloat = do whole    <- many1 (oneOf "0123456789")
              point    <- char '.'
              fraction <- many (oneOf "0123456789")
              let parsed = read (whole ++ "." ++ fraction ++ "0")
              return $ LiFloat parsed

heeTerm :: Parser Term
heeTerm = heeName
-- <|> literal
   <|> heeQuote

heeCompose :: Parser Term
heeCompose = undefined

ws :: Parser ()
ws = skipMany space
