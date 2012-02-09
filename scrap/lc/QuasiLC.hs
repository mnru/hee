{-# LANGUAGE TemplateHaskell, RankNTypes #-}

module QuasiLC
 ( lc
 ) where

import qualified Language.Haskell.TH as TH
import Language.Haskell.TH.Quote (dataToQa, dataToExpQ, dataToPatQ, QuasiQuoter(..))
import Data.Data
import Data.Generics

import Parser (parse)
import Eval (Var(..), Exp(..))

antiVarE :: Var -> Maybe TH.ExpQ
antiVarE (AV v) = Just $ TH.varE $ TH.mkName v
antiVarE _      = Nothing

antiExpE :: Exp -> Maybe TH.ExpQ
antiExpE (AE v) = Just $ TH.varE $ TH.mkName v
antiExpE _      = Nothing

antiVarP :: Var -> Maybe TH.PatQ
antiVarP (AV v) = Just $ TH.varP $ TH.mkName v
antiVarP _      = Nothing

antiExpP :: Exp -> Maybe TH.PatQ
antiExpP (AE v) = Just $ TH.varP $ TH.mkName v
antiExpP _      = Nothing

lamE  :: String -> TH.ExpQ
lamE s = parse s >>=
  dataToExpQ (const Nothing `extQ` antiVarE
                            `extQ` antiExpE)

lamP  :: String -> TH.PatQ
lamP s = parse s >>=
  dataToPatQ (const Nothing `extQ` antiVarP
                            `extQ` antiExpP)

lc :: QuasiQuoter
lc = QuasiQuoter { quoteExp  = lamE
                 , quotePat  = lamP
                 , quoteType = undefined    -- @todo
                 , quoteDec  = undefined }  -- @todo
