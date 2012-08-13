import System.Environment
import Control.Applicative ((<$>), (<*), (<*>), (*>), pure)
import Data.Either (either)
import Data.List (intercalate)
import Data.Text (pack)

import Language.Hee.Syntax
import Language.Hee.Parser
import Language.Hee.Eval

main
  = putStrLn =<< show . result . parse <$> input
  where
    input  = pack . intercalate " " <$> getArgs
    result = either (const $ Failure EEmpty []) (flip eval [])
    parse  = parseOnly parseExpr
