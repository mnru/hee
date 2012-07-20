import System.Environment
import Control.Applicative ((<$>), (<*), (<*>), (*>), pure)
import Data.Attoparsec.Text (parseOnly)
import Data.Either (either)
import Data.List (intercalate)
import Data.Text (Text, pack)

import Language.Hee.Parser
import Language.Hee.Terms

main
  = putStrLn =<< toStr . parse <$> input
  where
    input = pack . intercalate " " <$> getArgs
    parse = parseOnly heeTerm
    toStr = either id show
