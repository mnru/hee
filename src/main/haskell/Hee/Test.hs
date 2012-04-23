import Hee
import Hee.Checker
import Hee.Unification
import Hee.Substitution
import Hee.Parser
import Hee.Types
import Hee.Kinds
import System.Environment

typeOf :: String -> Either String String
typeOf s =
  do e <- Hee.Parser.heeTest s
     t <- Hee.Checker.checkTerm e
     let t' = Hee.Substitution.normalizeType t
     return $ showType t'

main = do args <- getArgs
          sequence $ map (\t -> do putStrLn . show $ t
                                   putStrLn . show $ either id id $ typeOf t
                                   putStrLn "")
                         (tests ++ args)
  where tests = [--"swap dup"
                --,"quote dup"
                ]

-- Neat: the type checker only needs to propogate constraints between
-- the 'up-to-now' type and the type of 'next' term. You could build
-- an IDE which annotates the type of a function as you type.
--
--   -- A (B -> C) (C -> D) -> A (B -> D)
--   x := compose
--
--   -- A (A -> B) (B -> C) -> C
--   x := compose apply
--
--   -- A (A -> B) (B -> C d) -> C d d
--   x := compose apply dup
