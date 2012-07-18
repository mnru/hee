{-# LANGUAGE QuasiQuotes #-}

import QuasiLC (lc)
import Eval

subst' [lc|$exp:e $exp:f|] x y =
  let e' = subst' e x y
      f' = subst' f x y
  in [lc|$exp:e' $exp:f'|]

main = putStrLn (show [lc|(\x.x) x|]) >>
       putStrLn (show [lc|(\x.x x)(\y.y y)|])
