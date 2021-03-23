{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module AMQP.Generator
  ( main,
  )
where

import Control.Monad
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import System.Environment
import System.Exit
import Text.Read (readMaybe)
import Text.Show.Pretty
import Text.XML as XML
import Text.XML.Cursor as Cursor

main :: IO ()
main = do
  args <- getArgs
  case args of
    (specFile : _) -> generateFrom specFile
    _ -> die "Supply the spec XML file as a command-line argument"

generateFrom :: FilePath -> IO ()
generateFrom fp = do
  xmlDoc <- XML.readFile def fp
  mapM_ pPrint (parseConstants xmlDoc)
  mapM_ pPrint (parseDomainTypes xmlDoc)

nodeElement :: Node -> Maybe Element
nodeElement = \case
  NodeElement e -> Just e
  _ -> Nothing

nodeContent :: Node -> Maybe Text
nodeContent = \case
  NodeContent t -> Just t
  _ -> Nothing

parseConstants :: Document -> [Constant]
parseConstants =
  mapMaybe parseConstant
    . mapMaybe nodeElement
    . elementNodes
    . documentRoot

parseDomainTypes :: Document -> [DomainType]
parseDomainTypes =
  mapMaybe parseDomainType
    . mapMaybe nodeElement
    . elementNodes
    . documentRoot

data AMQPSpec = AMQPSpec
  { amqpSpecConstants :: ![Constant]
  }
  deriving (Show, Eq, Generic)

data Constant = Constant
  { constantName :: !Text,
    constantValue :: !Word,
    constantClass :: !Text,
    constantDoc :: !(Maybe Text)
  }
  deriving (Show, Eq, Generic)

parseConstant :: Element -> Maybe Constant
parseConstant e = do
  guard $ elementName e == "constant"
  constantName <- M.lookup "name" $ elementAttributes e
  constantValueText <- M.lookup "value" $ elementAttributes e
  constantValue <- readMaybe (T.unpack constantValueText)
  constantClass <- M.lookup "class" $ elementAttributes e
  let constantDoc = parseDocUnder e
  pure Constant {..}

data DomainType = DomainType
  { domainTypeName :: !Text,
    domainTypeType :: !Text,
    domainTypeLabel :: !Text,
    domainTypeDoc :: !(Maybe Text),
    domainTypeAssertions :: ![Assertion]
  }
  deriving (Show, Eq, Generic)

parseDomainType :: Element -> Maybe DomainType
parseDomainType e = do
  guard $ elementName e == "domain"
  domainTypeName <- M.lookup "name" $ elementAttributes e
  domainTypeType <- M.lookup "type" $ elementAttributes e
  domainTypeLabel <- M.lookup "label" $ elementAttributes e
  let domainTypeDoc = parseDocUnder e
  let domainTypeAssertions = mapMaybe parseAssertion (mapMaybe nodeElement $ elementNodes e)
  pure DomainType {..}

data Assertion = AssertNotNull | AssertLength !Word | AssertRegex !Text
  deriving (Show, Eq, Generic)

parseAssertion :: Element -> Maybe Assertion
parseAssertion e = do
  guard $ elementName e == "assert"
  check <- M.lookup "check" $ elementAttributes e
  case check of
    "length" -> do
      valueText <- M.lookup "value" (elementAttributes e)
      AssertLength <$> readMaybe (T.unpack valueText)
    "regexp" -> do
      valueText <- M.lookup "value" (elementAttributes e)
      AssertRegex <$> readMaybe (T.unpack valueText)
    "notnull" -> pure AssertNotNull

data Class = Class
  { className :: !Text,
    classHandler :: !Text,
    classIndex :: !Word,
    classlabel :: !Text,
    classDoc :: !(Maybe Text),
    classFields :: ![Field],
    classMethods :: ![Method]
  }
  deriving (Show, Eq, Generic)

data Method = Method
  { methodName :: !Text,
    methodSynchronous :: !Bool,
    methodIndex :: !Word,
    methodLabel :: !Text,
    methodDoc :: !(Maybe Text),
    methodArguments :: ![Field],
    methodResponse :: !Text
  }
  deriving (Show, Eq, Generic)

data Field = Field
  { fieldName :: !Text,
    fieldDomain :: !Text,
    fieldLabel :: !Text,
    fieldDoc :: !(Maybe Text)
  }
  deriving (Show, Eq, Generic)

parseDocUnder :: Element -> Maybe Text
parseDocUnder =
  fmap stripDoc
    . listToMaybe
    . mapMaybe nodeContent
    . concatMap elementNodes
    . filter ((== "doc") . elementName)
    . mapMaybe nodeElement
    . elementNodes

stripDoc :: Text -> Text
stripDoc = T.strip . T.unlines . map T.strip . T.lines
