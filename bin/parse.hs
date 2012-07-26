import System.Environment
import Control.Applicative ((<$>), (<*), (<*>), (*>), pure)
import Data.Either (either)
import Data.List (intercalate)
import Data.Text (pack)

import Language.Hee.Parser

main
  = putStrLn =<< toStr . parse <$> input
  where
    input = pack . intercalate " " <$> getArgs
    toStr = either show show
    parse = parseOnly parseExpr
