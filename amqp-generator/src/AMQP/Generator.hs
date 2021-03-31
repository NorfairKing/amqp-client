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
import System.FilePath
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
  let modules :: [(FilePath, Doc)]
      modules =
        [ ("amqp-serialisation/src/AMQP/Serialisation/Generated/Constants.hs", genGeneratedConstantsModule spec),
          ("amqp-serialisation/src/AMQP/Serialisation/Generated/DomainTypes.hs", genGeneratedTypesModule spec),
          ("amqp-serialisation-gen/src/AMQP/Serialisation/Generated/Methods/Gen.hs", genGeneratedGeneratorsModule spec),
          ("amqp-serialisation/src/AMQP/Serialisation/Generated/Methods.hs", genGeneratedMethodsModule spec)
        ]
  forM_ modules $ \(path, m) -> do
    createDirectoryIfMissing True $ takeDirectory path
    let moduleString = render $ to_HPJ_Doc m
    writeFile path moduleString
    runProcess_ $ proc "ormolu" ["--mode", "inplace", path]
    result <- readFile path
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
      genMethodSumType amqpSpecClasses, -- TODO move this back down when we finish the parsing code
      genClassesTypesDoc amqpSpecClasses
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
genGrammarComment Grammar {..} = vcat [comment "Grammar:", comment "\n", blockComment grammarText]

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
classMethodTypeDecs className classIndex m@AMQP.Method {..} =
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
            FunD (mkName "methodMethodId") [Clause [ConP (mkName "Proxy") []] (NormalB (LitE (IntegerL (toInteger methodIndex)))) []],
            FunD
              (mkName "methodSynchronous")
              [Clause [ConP (mkName "Proxy") []] (NormalB (ConE $ mkName (if methodSynchronous then "True" else "False"))) []],
            FunD
              (mkName "parseMethodArguments")
              [genParseMethodArguments className m]
          ],
        InstanceD
          Nothing
          []
          (AppT (ConT (mkName "FromMethod")) (VarT n))
          [ FunD
              (mkName "fromMethod")
              [ Clause
                  []
                  ( NormalB
                      ( LamCaseE $
                          let varName = mkName "m"
                           in [ Match (ConP (mkMethodSumTypeConstructorName className methodName) [VarP varName]) (NormalB (AppE (ConE (mkName "Just")) (VarE varName))) [],
                                Match WildP (NormalB (ConE (mkName "Nothing"))) []
                              ]
                      )
                  )
                  []
              ]
          ]
      ]

-- ++ [ undefined | methodSynchronous
--    ]

genParseMethodArguments :: Text -> Method -> Clause
genParseMethodArguments className AMQP.Method {..} =
  Clause
    []
    ( NormalB
        ( DoE $
            concat
              [ map
                  ( \case
                      NonBitField Field {..} ->
                        BindS
                          (VarP (mkMethodFieldTypeVarName className methodName fieldName))
                          ( VarE (mkName "parseArgument")
                          )
                      BitFields [Field {..}] ->
                        BindS
                          (VarP (mkMethodFieldTypeVarName className methodName fieldName))
                          ( VarE (mkName "parseArgument")
                          )
                      BitFields fs ->
                        BindS
                          (TupP (map (\Field {..} -> VarP (mkMethodFieldTypeVarName className methodName fieldName)) fs))
                          (VarE (parseNBitsFunctionName (length fs)))
                  )
                  (groupFields methodArguments),
                [ NoBindS
                    ( AppE
                        (VarE (mkName "pure"))
                        ( if null methodArguments -- No need for the extra braces.
                            then ConE (mkMethodTypeName className methodName)
                            else
                              RecConE
                                (mkMethodTypeName className methodName)
                                ( map
                                    ( \Field {..} ->
                                        ( mkMethodFieldTypeName className methodName fieldName,
                                          VarE
                                            ( mkMethodFieldTypeVarName className methodName fieldName
                                            )
                                        )
                                    )
                                    methodArguments
                                )
                        )
                    )
                ]
              ]
        )
    )
    []

groupFields :: [AMQP.Field] -> [GroupedField]
groupFields = go
  where
    go :: [AMQP.Field] -> [GroupedField]
    go [] = []
    go (f : fs) = if fieldIsBit f then goBits [f] fs else NonBitField f : go fs

    goBits :: [AMQP.Field] -> [AMQP.Field] -> [GroupedField]
    goBits acc [] = [BitFields (reverse acc)]
    goBits acc (f : fs) =
      if fieldIsBit f
        then goBits (f : acc) fs
        else BitFields (reverse acc) : go (f : fs)

-- TODO we probably want to do this more robustly by using the "type" attribute on the domain types that we've parsed instead.
fieldIsBit :: Field -> Bool
fieldIsBit Field {..} = case fromMaybe (error "A field must have either a type or a domain type") $ fieldType <|> fieldDomain of
  "bit" -> True
  "no-ack" -> True
  "no-local" -> True
  "no-wait" -> True
  "redelivered" -> True
  _ -> False

data GroupedField = NonBitField Field | BitFields [Field]
  deriving (Show, Eq)

parseNBitsFunctionName :: Int -> Name
parseNBitsFunctionName n = mkName $ concat ["parse", show n, "Bits"]

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

mkMethodFieldTypeVarName :: Text -> Text -> Text -> Name
mkMethodFieldTypeVarName className methodName fieldName = mkHaskellVarName (T.intercalate "-" [className, methodName, fieldName, "parsed"])

genMethodSumType :: [AMQP.Class] -> Doc
genMethodSumType cs =
  vcat
    [ haddockIntro "A sum type of all the methods",
      ppr_list $ methodsSumTypeDecs cs,
      haddockIntro "A type class of things that could be in a method frame",
      text "class FromMethod a where",
      text "  fromMethod :: Method -> Maybe a",
      haddockIntro "Turn a 'Method' into a 'ByteString.Builder'.",
      ppr_list $ genBuildSumTypeFunction cs,
      haddockIntro "Parse a 'Method' frame payload.",
      ppr_list $ genParseSumTypeFunction cs,
      haddockIntro "Check if a 'Method' is synchronous.",
      ppr_list $ genMethodSynchronousFunction cs
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

parseSumFunctionMatchForClass :: AMQP.Class -> Match
parseSumFunctionMatchForClass AMQP.Class {..} =
  let methodIdVar = mkName "mid"
   in Match
        (LitP (IntegerL (toInteger classIndex)))
        ( NormalB
            ( CaseE
                (VarE methodIdVar)
                ( map (parseSumFunctionMatchForMethod className) classMethods
                    ++ [matchFailedMatch ("method id for class '" ++ T.unpack className ++ "' (" ++ show classIndex ++ ")") methodIdVar]
                )
            )
        )
        []

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

genMethodSynchronousFunction :: [AMQP.Class] -> [Dec]
genMethodSynchronousFunction cs =
  let n = mkName "methodIsSynchronous"
   in [ SigD n (AppT (AppT ArrowT (ConT (mkName "Method"))) (ConT (mkName "Bool"))),
        FunD
          n
          [ Clause
              []
              ( NormalB
                  ( LamCaseE
                      ( flip concatMap cs $ \Class {..} -> flip map classMethods $ \Method {..} ->
                          Match
                            ( ConP
                                (mkMethodSumTypeConstructorName className methodName)
                                [WildP]
                            )
                            ( NormalB
                                ( AppE
                                    (VarE (mkName "methodSynchronous"))
                                    (SigE (ConE (mkName "Proxy")) (AppT (ConT (mkName "Proxy")) (ConT (mkMethodTypeName className methodName))))
                                )
                            )
                            []
                      )
                  )
              )
              []
          ]
      ]

genGeneratedGeneratorsModule :: AMQPSpec -> Doc
genGeneratedGeneratorsModule AMQPSpec {..} =
  vcat
    [ text "{-# OPTIONS_GHC -fno-warn-orphans #-}",
      text "module AMQP.Serialisation.Generated.Methods.Gen where",
      text "",
      text "import Data.GenValidity",
      text "import AMQP.Serialisation.Generated.Methods",
      text "import AMQP.Serialisation.Base.Gen ()",
      text "",
      genGeneratorsDoc amqpSpecClasses
    ]

genGeneratorsDoc :: [AMQP.Class] -> Doc
genGeneratorsDoc cs =
  ppr_list $ concatMap genGeneratorInstances cs

genGeneratorInstances :: AMQP.Class -> [Dec]
genGeneratorInstances AMQP.Class {..} = concatMap (genGeneratorInstance className) classMethods

genGeneratorInstance :: Text -> AMQP.Method -> [Dec]
genGeneratorInstance className Method {..} =
  [ InstanceD
      Nothing
      []
      (AppT (ConT (mkName "GenValid")) (ConT (mkMethodTypeName className methodName)))
      [ FunD
          (mkName "genValid")
          [ Clause [] (NormalB (VarE (mkName "genValidStructurallyWithoutExtraChecking"))) []
          ],
        FunD
          (mkName "shrinkValid")
          [ Clause [] (NormalB (VarE (mkName "shrinkValidStructurallyWithoutExtraFiltering"))) []
          ]
      ]
  ]
