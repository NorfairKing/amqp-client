{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module AMQP.Generator
  ( main,
  )
where

import AMQP.Generator.Parse as AMQP hiding (Doc (..))
import qualified AMQP.Generator.Parse as AMQP
import Control.Monad
import Data.Either (partitionEithers)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Language.Haskell.TH.Lib
import Language.Haskell.TH.Ppr
import Language.Haskell.TH.PprLib
import Language.Haskell.TH.Syntax
import System.Environment
import System.Exit
import Text.Casing
import Text.PrettyPrint (render)
import Text.Read (readMaybe)
import Text.Show.Pretty (pPrint, ppShow)

main :: IO ()
main = do
  args <- getArgs
  case args of
    (specFile : _) -> generateFrom specFile
    _ -> die "Supply the spec XML file as a command-line argument"

generateFrom :: FilePath -> IO ()
generateFrom fp = do
  spec <- AMQP.parseSpecFromFile fp
  pPrint spec
  putStrLn $ render $ to_HPJ_Doc $ genConstantsDoc $ amqpSpecConstants spec

genConstantsDoc :: [Constant] -> Doc
genConstantsDoc = vcat . map genConstantDoc

genConstantDoc :: Constant -> Doc
genConstantDoc c@AMQP.Constant {..} =
  vcat
    [ genHaddocks constantName constantDoc,
      ppr_list (constantDecs c)
    ]

constantDecs :: Constant -> [Dec]
constantDecs AMQP.Constant {..} =
  let n = mkHaskellName constantName
   in [ SigD n (ConT (mkName "Word")),
        FunD
          n
          [ Clause [] (NormalB (LitE (IntegerL (fromIntegral constantValue)))) []
          ]
      ]

genHaddocks :: Text -> Maybe AMQP.Doc -> Doc
genHaddocks intro mDoc =
  vcat
    [ haddockIntro intro,
      case mDoc of
        Nothing -> empty
        Just d -> comment " " $$ genDocComment d
    ]

genMDocComment :: Maybe AMQP.Doc -> Doc
genMDocComment = maybe empty genDocComment

genDocComment :: AMQP.Doc -> Doc
genDocComment = comment . AMQP.docText

haddockIntro :: Text -> Doc
haddockIntro t = text "-- |" <+> text (T.unpack t)

comment :: Text -> Doc
comment = vcat . map ((text "--" <+>) . text) . lines . T.unpack

mkHaskellName :: Text -> Name
mkHaskellName = mkName . toCamel . fromKebab . T.unpack
