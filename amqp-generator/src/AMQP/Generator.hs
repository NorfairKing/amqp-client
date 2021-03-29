{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module AMQP.Generator
  ( main,
  )
where

import AMQP.Generator.Parse as AMQP hiding (Doc (..))
import qualified AMQP.Generator.Parse as AMQP
import Control.Applicative ((<|>))
import Control.Monad
import Data.Either (partitionEithers)
import Data.List
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
  let moduleString = render $ to_HPJ_Doc $ genGeneratedModule spec
  putStrLn moduleString
  writeFile "amqp-serialisation/src/AMQP/Serialisation/Generated.hs" moduleString

genGeneratedModule :: AMQP.AMQPSpec -> Doc
genGeneratedModule AMQP.AMQPSpec {..} =
  vcat
    [ text "{-# LANGUAGE DeriveGeneric #-}",
      text "module AMQP.Serialisation.Generated where",
      text "",
      text "import AMQP.Serialisation.Base",
      text "import GHC.Generics (Generic)",
      text "import Data.Word",
      text "",
      genConstantsDoc amqpSpecConstants,
      genDomainTypesDoc amqpSpecDomainTypes,
      genClassesTypesDoc amqpSpecClasses
    ]

genConstantsDoc :: [Constant] -> Doc
genConstantsDoc = vcat . intersperse (text "") . map genConstantDoc

genConstantDoc :: Constant -> Doc
genConstantDoc c@AMQP.Constant {..} =
  vcat
    [ genHaddocks constantName constantDoc,
      ppr_list (constantDecs c)
    ]

constantDecs :: Constant -> [Dec]
constantDecs AMQP.Constant {..} =
  let n = mkHaskellVarName constantName
   in [ SigD n (ConT (makeConstantType constantValue)),
        FunD
          n
          [ Clause [] (NormalB (LitE (IntegerL (fromIntegral constantValue)))) []
          ]
      ]

makeConstantType :: Word -> Name
makeConstantType w
  | w < 256 = mkName "Word8"
  | otherwise = mkName "Word"

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

mkHaskellVarName :: Text -> Name
mkHaskellVarName = mkName . remapReserved . toCamel . fromKebab . T.unpack
  where
    remapReserved = \case
      "type" -> "typ"
      "class" -> "clazz"
      s -> s

genDomainTypesDoc :: [DomainType] -> Doc
genDomainTypesDoc = vcat . intersperse (text "") . mapMaybe genDomainTypeDoc

genDomainTypeDoc :: DomainType -> Maybe Doc
genDomainTypeDoc dt@AMQP.DomainType {..} =
  if domainTypeName == domainTypeType
    then Nothing -- This means it's one of the base types
    else
      Just $
        vcat
          [ genHaddocks (maybe domainTypeName (\l -> T.concat [domainTypeName, ": ", l]) domainTypeLabel) domainTypeDoc,
            ppr_list (domainTypeDecs dt)
          ]

domainTypeDecs :: DomainType -> [Dec]
domainTypeDecs DomainType {..} =
  [ TySynD (mkHaskellTypeName domainTypeName) [] (ConT (typeTranslator domainTypeType))
  ]

typeTranslator :: Text -> Name
typeTranslator = \case
  "bit" -> mkName "Bit"
  "octet" -> mkName "Octet"
  "short" -> mkName "ShortUInt"
  "long" -> mkName "LongUInt"
  "longlong" -> mkName "LongLongUInt"
  "shortstr" -> mkName "ShortString"
  "longstr" -> mkName "LongString"
  "table" -> mkName "FieldTable"
  t -> mkHaskellTypeName t

mkHaskellTypeName :: Text -> Name
mkHaskellTypeName = mkName . toPascal . fromKebab . T.unpack

genClassesTypesDoc :: [Class] -> Doc
genClassesTypesDoc = vcat . intersperse (text "\n") . map genClassTypesDoc

genClassTypesDoc :: Class -> Doc
genClassTypesDoc c@AMQP.Class {..} = vcat $ intersperse (text "") $ map (genClassMethodTypeDoc className) classMethods

genClassMethodTypeDoc :: Text -> Method -> Doc
genClassMethodTypeDoc className m@AMQP.Method {..} =
  vcat
    [ genHaddocks (T.unwords [T.concat ["'", methodName, "':"], methodLabel]) methodDoc,
      ppr_list (classMethodTypeDecs className m)
    ]

classMethodTypeDecs :: Text -> Method -> [Dec]
classMethodTypeDecs className AMQP.Method {..} =
  let n = mkMethodTypeName $ T.intercalate "-" [className, methodName]
   in [ DataD
          []
          n
          []
          Nothing
          [ if null methodArguments
              then NormalC n [] -- No need to generate the extra braces
              else
                RecC
                  n
                  (map (classMethodFieldVarBangType className methodName) methodArguments)
          ]
          [ DerivClause Nothing [ConT (mkName "Show"), ConT (mkName "Eq"), ConT (mkName "Generic")]
          ]
      ]

-- QUESTION: Should we unpack method fields?
-- ANSWER: Let's try and see what happens.
classMethodFieldVarBangType :: Text -> Text -> Field -> VarBangType
classMethodFieldVarBangType className method AMQP.Field {..} =
  ( mkHaskellVarName (T.intercalate "-" [className, method, fieldName]),
    Bang SourceUnpack SourceStrict,
    -- This 'fromMaybe' should not be necessary, refactor it away?
    ConT (typeTranslator (fromMaybe (error "A field must have either a type or a domain type") $ fieldType <|> fieldDomain))
  )

mkMethodTypeName :: Text -> Name
mkMethodTypeName = mkHaskellTypeName
