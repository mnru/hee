import System.Environment
import Control.Applicative ((<$>))
import Data.List (intercalate)
import Data.Text (pack)

import Language.Hee.Syntax
import Language.Hee.Parser
import Language.Hee.Eval

main :: IO ()
main
  = putStrLn =<< printResult . result . parse <$> input
  where
    input  = pack . intercalate " " <$> getArgs
    parse  = parseOnly parseExpr
    result = either (const $ Failure EEmpty []) (`eval` [])

    printResult (Success _ s) = printStack s
    printResult (Failure e s) = "(" ++ show e ++ ", " ++ printStack s ++ ")"
    printStack s              = intercalate " " $ map show (reverse s)
