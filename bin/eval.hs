{-# LANGUAGE OverloadedStrings #-}

import Prelude hiding (getContents)
import Control.Applicative ((<$>))
import Data.Text (pack, unpack, append)
import Data.Text.IO (getContents)
import Data.List (intercalate)

import Language.Hee.Parser
import Language.Hee.Eval

main :: IO ()
main
  = putStrLn =<< printResult . result . parse <$> input
  where
    input   = getContents
    parse   = parseOnly parseFile
    result  = either parseFail runFile
    parseFail e = ((Left (append "parse failure: " (pack $ show e)), []), [])
    printResult ((Left err, _), s) = unpack err ++ ": " ++ printStack s
    printResult ((Right  _, _), s) = printStack s
    printStack s                   = intercalate " " $ map show (reverse s)
