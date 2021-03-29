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
import Data.List
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Language.Haskell.TH.Lib
import Language.Haskell.TH.Ppr
import Language.Haskell.TH.PprLib
import Language.Haskell.TH.Syntax
import System.Directory
import System.Environment
import System.Exit
import System.Process.Typed
import Text.Casing
import Text.PrettyPrint (render)

main :: IO ()
main = do
  args <- getArgs
  case args of
    (specFile : _) -> generateFrom specFile
    _ -> die "Supply the spec XML file as a command-line argument"

generateFrom :: FilePath -> IO ()
generateFrom inPath = do
  spec <- AMQP.parseSpecFromFile inPath
  let generatedDir = "amqp-serialisation/src/AMQP/Serialisation/Generated/"
  let modules :: [(FilePath, Doc)]
      modules =
        [ ("Constants.hs", genGeneratedConstantsModule spec),
          ("DomainTypes.hs", genGeneratedTypesModule spec),
          ("Methods.hs", genGeneratedMethodsModule spec)
        ]
  createDirectoryIfMissing True generatedDir
  forM_ modules $ \(name, m) -> do
    let moduleString = render $ to_HPJ_Doc m
    let outPath = generatedDir ++ name
    writeFile outPath moduleString
    runProcess_ $ proc "ormolu" ["--mode", "inplace", outPath]
    result <- readFile outPath
    putStrLn result

genGeneratedConstantsModule :: AMQP.AMQPSpec -> Doc
genGeneratedConstantsModule AMQP.AMQPSpec {..} =
  vcat
    [ text "module AMQP.Serialisation.Generated.Constants where",
      text "",
      text "import Data.Word",
      text "",
      genConstantsDoc amqpSpecConstants
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

genGeneratedTypesModule :: AMQP.AMQPSpec -> Doc
genGeneratedTypesModule AMQP.AMQPSpec {..} =
  vcat
    [ text "module AMQP.Serialisation.Generated.DomainTypes where",
      text "",
      text "import AMQP.Serialisation.Base",
      text "",
      genDomainTypesDoc amqpSpecDomainTypes
    ]

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

genGeneratedMethodsModule :: AMQP.AMQPSpec -> Doc
genGeneratedMethodsModule AMQP.AMQPSpec {..} =
  vcat
    [ text "{-# LANGUAGE DeriveGeneric #-}",
      text "{-# LANGUAGE LambdaCase #-}",
      text "module AMQP.Serialisation.Generated.Methods where",
      text "",
      text "import AMQP.Serialisation.Argument",
      text "import AMQP.Serialisation.Base",
      text "import AMQP.Serialisation.Frame",
      text "import AMQP.Serialisation.Generated.DomainTypes",
      text "import Data.Attoparsec.ByteString as Parse",
      text "import Data.ByteString.Builder as ByteString (Builder)",
      text "import GHC.Generics (Generic)",
      text "import Data.Proxy",
      text "import Data.Validity",
      text "",
      genClassesTypesDoc amqpSpecClasses,
      genMethodSumType amqpSpecClasses
    ]

genHaddocks :: Text -> Maybe AMQP.Doc -> Doc
genHaddocks intro mDoc =
  vcat
    [ haddockIntro intro,
      case mDoc of
        Nothing -> empty
        Just d -> comment " " $$ genDocComment d
    ]

genDocComment :: AMQP.Doc -> Doc
genDocComment = comment . AMQP.docText

genGrammarComment :: Grammar -> Doc
genGrammarComment Grammar {..} = vcat [comment "Grammar:", comment "", blockComment grammarText]

sectionIntro :: Text -> Doc
sectionIntro t = text "-- *" <+> text (T.unpack t)

haddockIntro :: Text -> Doc
haddockIntro t = text "-- |" <+> text (T.unpack t)

comment :: Text -> Doc
comment = vcat . map ((text "--" <+>) . text) . lines . T.unpack

blockComment :: Text -> Doc
blockComment = vcat . map ((text "-- >" <+>) . text) . lines . T.unpack

mkHaskellVarName :: Text -> Name
mkHaskellVarName = mkName . remapReserved . toCamel . fromKebab . T.unpack
  where
    remapReserved = \case
      "type" -> "typ"
      "class" -> "clazz"
      s -> s

genClassesTypesDoc :: [Class] -> Doc
genClassesTypesDoc = vcat . intersperse (text "\n") . map genClassTypesDoc

genClassTypesDoc :: Class -> Doc
genClassTypesDoc AMQP.Class {..} =
  vcat
    [ sectionIntro $ T.unwords ["The", T.concat ["@", className, "@"], "class"],
      comment "",
      maybe empty genDocComment classDoc,
      comment "\n",
      maybe empty genGrammarComment classGrammar,
      text "",
      vcat $
        intersperse (text "") $
          map (genClassMethodTypeDoc className classIndex) classMethods
    ]

genClassMethodTypeDoc :: Text -> Word -> Method -> Doc
genClassMethodTypeDoc className classIndex m@AMQP.Method {..} =
  vcat
    [ genHaddocks (T.unwords [T.concat ["The @", methodName, "@ method:"], methodLabel]) methodDoc,
      ppr_list (classMethodTypeDecs className classIndex m)
    ]

classMethodTypeDecs :: Text -> Word -> Method -> [Dec]
classMethodTypeDecs className classIndex AMQP.Method {..} =
  let n = mkMethodTypeName className methodName
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
          ],
        InstanceD Nothing [] (AppT (ConT (mkName "Validity")) (VarT n)) [],
        InstanceD
          Nothing
          []
          (AppT (ConT (mkName "IsMethod")) (VarT n))
          [ FunD (mkName "methodClassId") [Clause [ConP (mkName "Proxy") []] (NormalB (LitE (IntegerL (toInteger classIndex)))) []],
            FunD (mkName "methodMethodId") [Clause [ConP (mkName "Proxy") []] (NormalB (LitE (IntegerL (toInteger methodIndex)))) []]
          ]
      ]

mkMethodTypeName :: Text -> Text -> Name
mkMethodTypeName className methodName = mkHaskellTypeName $ T.intercalate "-" [className, methodName]

-- QUESTION: Should we unpack method fields?
-- ANSWER: Maybe, but it doesn't work on every field so let's revisit this later.
classMethodFieldVarBangType :: Text -> Text -> Field -> VarBangType
classMethodFieldVarBangType className methodName AMQP.Field {..} =
  ( mkMethodFieldTypeName className methodName fieldName,
    Bang NoSourceUnpackedness SourceStrict,
    -- This 'fromMaybe' should not be necessary, refactor it away?
    ConT (typeTranslator (fromMaybe (error "A field must have either a type or a domain type") $ fieldType <|> fieldDomain))
  )

mkMethodFieldTypeName :: Text -> Text -> Text -> Name
mkMethodFieldTypeName className methodName fieldName = mkHaskellVarName (T.intercalate "-" [className, methodName, fieldName])

genMethodSumType :: [AMQP.Class] -> Doc
genMethodSumType cs =
  vcat
    [ haddockIntro "A sum type of all the methods",
      ppr_list $ methodsSumTypeDecs cs,
      haddockIntro "Turn a 'Method' into a 'ByteString.Builder'.",
      ppr_list $ genBuildSumTypeFunction cs,
      haddockIntro "Parse a 'Method' frame payload.",
      ppr_list $ genParseSumTypeFunction cs
    ]

methodsSumTypeDecs :: [AMQP.Class] -> [Dec]
methodsSumTypeDecs cs =
  let n = mkName "Method"
   in [ DataD
          []
          n
          []
          Nothing
          (concatMap classSumTypeConstructors cs)
          [ DerivClause
              Nothing
              [ ConT (mkName "Show"),
                ConT (mkName "Eq"),
                ConT (mkName "Generic")
              ]
          ],
        InstanceD
          Nothing
          []
          (AppT (ConT (mkName "Validity")) (VarT n))
          []
      ]

classSumTypeConstructors :: AMQP.Class -> [Con]
classSumTypeConstructors AMQP.Class {..} = map (methodSumTypeConstructor className) classMethods

methodSumTypeConstructor :: Text -> AMQP.Method -> Con
methodSumTypeConstructor className AMQP.Method {..} =
  NormalC
    (mkMethodSumTypeConstructorName className methodName)
    [ (Bang NoSourceUnpackedness SourceStrict, ConT (mkMethodTypeName className methodName))
    ]

mkMethodSumTypeConstructorName :: Text -> Text -> Name
mkMethodSumTypeConstructorName className methodName = mkHaskellTypeName $ T.intercalate "-" ["method", className, methodName]

genBuildSumTypeFunction :: [AMQP.Class] -> [Dec]
genBuildSumTypeFunction cs =
  let n = mkName "buildMethodFramePayload"
   in [ SigD n (AppT (AppT ArrowT (ConT (mkName "Method"))) (ConT (mkName "ByteString.Builder"))),
        FunD
          n
          [ Clause
              []
              ( NormalB
                  ( LamCaseE
                      ( flip concatMap cs $ \Class {..} -> flip map classMethods $ \Method {..} ->
                          let varName = mkName "m"
                           in Match
                                ( ConP
                                    (mkMethodSumTypeConstructorName className methodName)
                                    [VarP varName]
                                )
                                ( NormalB
                                    ( AppE
                                        (VarE (mkName "buildGivenMethodFramePayload"))
                                        (VarE varName)
                                    )
                                )
                                []
                      )
                  )
              )
              []
          ]
      ]

genParseSumTypeFunction :: [AMQP.Class] -> [Dec]
genParseSumTypeFunction cs =
  let n = mkName "parseMethodFramePayload"
   in [ SigD n (AppT (ConT (mkName "Parser")) (ConT (mkName "Method"))),
        FunD
          n
          [ Clause
              []
              ( NormalB
                  ( AppE
                      ( VarE
                          ( mkName "parseMethodFramePayloadHelper"
                          )
                      )
                      ( let classIdVar = mkName "cid"
                            methodIdVar = mkName "mid"
                         in LamE
                              [VarP classIdVar, VarP methodIdVar]
                              ( CaseE
                                  (VarE classIdVar)
                                  ( map parseSumFunctionMatchForClass cs
                                      ++ [matchFailedMatch "class id" classIdVar]
                                  )
                              )
                      )
                  )
              )
              []
          ]
      ]

-- TODO make exhaustive matches
parseSumFunctionMatchForClass :: AMQP.Class -> Match
parseSumFunctionMatchForClass AMQP.Class {..} =
  let methodIdVar = mkName "mid"
   in Match
        (LitP (IntegerL (toInteger classIndex)))
        ( NormalB
            ( CaseE
                (VarE methodIdVar)
                ( map (parseSumFunctionMatchForMethod className) classMethods
                    ++ [matchFailedMatch ("method id for class " ++ T.unpack className ++ " (" ++ show classIndex ++ ")") methodIdVar]
                )
            )
        )
        []

-- TODO make exhaustive matches
parseSumFunctionMatchForMethod :: Text -> AMQP.Method -> Match
parseSumFunctionMatchForMethod className AMQP.Method {..} =
  Match
    (LitP (IntegerL (toInteger methodIndex)))
    ( NormalB
        ( InfixE
            (Just (VarE (mkMethodSumTypeConstructorName className methodName)))
            (VarE (mkName "<$>"))
            (Just (VarE (mkName "parseMethodArguments")))
        )
    )
    []

matchFailedMatch :: String -> Name -> Match
matchFailedMatch thing var =
  Match
    WildP
    ( NormalB
        ( AppE
            (VarE (mkName "fail"))
            ( InfixE
                ( Just
                    ( LitE
                        ( StringL $ "Unknown " ++ thing
                        )
                    )
                )
                (VarE (mkName "++"))
                (Just (AppE (VarE (mkName "show")) (VarE var)))
            )
        )
    )
    []
