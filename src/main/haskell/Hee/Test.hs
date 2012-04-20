import Hee
import Hee.Checker
import Hee.Unification
import Hee.Substitution
import Hee.Parser
import Hee.Types

typeOf s =
  do e <- Hee.Parser.heeTest s
     t <- Hee.Checker.checkTerm e
     let t' = Hee.Substitution.normalizeType t
     return $ showType t'

main = sequence $ map (\t -> print (t, either id id $ typeOf t)) tests
  where tests = ["dup pop"
                ,"pop pop"
                ,"dup dup"
                ,"swap swap"]
