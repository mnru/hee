import Control.Applicative ((<$>))
import Data.Text.IO (getContents)

import Prelude hiding (getContents)
import Language.Hee.Parser

main :: IO ()
main
  = putStrLn =<< toStr . parse <$> input
  where
    input = getContents
    toStr = either show show
    parse = parseOnly parseFile
