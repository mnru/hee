import System.Environment
import Control.Applicative ((<$>), (<*), (<*>), (*>), pure)
import Data.Attoparsec.Text (parseOnly)
import Data.Either (either)
import Data.List (intercalate)
import Data.Text (pack)

import Language.Hee.Syntax

main
  = putStrLn =<< toStr . parse <$> input
  where
    input = pack . intercalate " " <$> getArgs
    parse = parseOnly parseExpr
    toStr = either id show
