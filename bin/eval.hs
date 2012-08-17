import Prelude hiding (getContents)
import Control.Applicative ((<$>))
import Data.Text.IO (getContents)
import Data.List (intercalate)

import Language.Hee.Syntax
import Language.Hee.Parser
import Language.Hee.Eval

main :: IO ()
main
  = putStrLn =<< printResult . result . parse <$> input
  where
    input  = getContents
    parse  = parseOnly parseExpr
    result = either (const $ Failure EEmpty []) (`eval` [])

    printResult (Success _ s) = printStack s
    printResult (Failure e s) = "(" ++ show e ++ ", " ++ printStack s ++ ")"
    printStack s              = intercalate " " $ map show (reverse s)
