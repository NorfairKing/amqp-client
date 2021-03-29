{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module AMQP.Generator.Parse where

import Data.Either
import qualified Data.Map as M
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import System.Exit
import Text.Read (readMaybe)
import Text.XML as XML

parseSpecFromFile :: FilePath -> IO AMQPSpec
parseSpecFromFile fp = do
  xmlDoc <- XML.readFile def fp
  case fromElement (documentRoot xmlDoc) of
    Left err -> die err
    Right spec -> pure spec

data AMQPSpec = AMQPSpec
  { amqpSpecConstants :: ![Constant],
    amqpSpecDomainTypes :: ![DomainType],
    amqpSpecClasses :: ![Class]
  }
  deriving (Show, Eq, Generic)

instance FromElement AMQPSpec where
  fromElement = elementWithName "amqp" $ \e ->
    AMQPSpec
      <$> elementsBelow e "constant"
      <*> elementsBelow e "domain"
      <*> elementsBelow e "class"

data Constant = Constant
  { constantName :: !Text,
    constantValue :: !Word,
    constantClass :: !(Maybe Text),
    constantDoc :: !(Maybe Doc)
  }
  deriving (Show, Eq, Generic)

instance FromElement Constant where
  fromElement = elementWithName "constant" $ \e ->
    Constant
      <$> e .: "name"
      <*> e .: "value"
      <*> e .:? "class"
      <*> firstElementBelow e "doc"

data DomainType = DomainType
  { domainTypeName :: !Text,
    domainTypeType :: !Text,
    domainTypeLabel :: !(Maybe Text),
    domainTypeDoc :: !(Maybe Doc),
    domainTypeAssertions :: ![Assertion]
  }
  deriving (Show, Eq, Generic)

instance FromElement DomainType where
  fromElement = elementWithName "domain" $ \e ->
    DomainType
      <$> e .: "name"
      <*> e .: "type"
      <*> e .:? "label"
      <*> firstElementBelow e "doc"
      <*> elementsBelow e "assert"

data Assertion = AssertNotNull | AssertLength !Word | AssertRegex !Text
  deriving (Show, Eq, Generic)

instance FromElement Assertion where
  fromElement = elementWithName "assert" $ \e -> do
    check <- e .: "check"
    case check :: Text of
      "length" -> AssertLength <$> e .: "value"
      "regexp" -> AssertRegex <$> e .: "value"
      "notnull" -> pure AssertNotNull
      _ -> Left $ "Unknown assertion check: " <> show check

data Class = Class
  { className :: !Text,
    classHandler :: !Text,
    classIndex :: !Word,
    classlabel :: !Text,
    classDoc :: !(Maybe Doc),
    classGrammar :: !(Maybe Grammar),
    classFields :: ![Field],
    classMethods :: ![Method]
  }
  deriving (Show, Eq, Generic)

instance FromElement Class where
  fromElement = elementWithName "class" $ \e ->
    Class
      <$> e .: "name"
      <*> e .: "handler"
      <*> e .: "index"
      <*> e .: "label"
      <*> firstElementBelow e "doc"
      <*> firstParsingElementBelow e "doc"
      <*> elementsBelow e "field"
      <*> elementsBelow e "method"

data Method = Method
  { methodName :: !Text,
    methodSynchronous :: !Bool,
    methodIndex :: !Word,
    methodLabel :: !Text,
    methodDoc :: !(Maybe Doc),
    methodArguments :: ![Field],
    methodResponses :: ![Response]
  }
  deriving (Show, Eq, Generic)

instance FromElement Method where
  fromElement = elementWithName "method" $ \e ->
    Method
      <$> e .: "name"
      <*> (fromMaybe False <$> (e .:? "synchronous"))
      <*> e .: "index"
      <*> e .: "label"
      <*> firstElementBelow e "doc"
      <*> elementsBelow e "field"
      <*> elementsBelow e "response"

newtype Response = Response {responseName :: Text}
  deriving (Show, Eq, Generic)

instance FromElement Response where
  fromElement = elementWithName "response" $ \e -> Response <$> e .: "name"

data Field = Field
  { fieldName :: !Text,
    fieldDomain :: !(Maybe Text),
    fieldType :: !(Maybe Text),
    fieldLabel :: !(Maybe Text),
    fieldDoc :: !(Maybe Doc)
  }
  deriving (Show, Eq, Generic)

instance FromElement Field where
  fromElement = elementWithName "field" $ \e ->
    Field
      <$> e .: "name"
      <*> e .:? "domain"
      <*> e .:? "type"
      <*> e .:? "label"
      <*> firstElementBelow e "doc"

newtype Grammar = Grammar {grammarText :: Text}
  deriving (Show, Eq, Generic)

instance FromElement Grammar where
  fromElement = elementWithName "doc" $ \e -> do
    t <- e .:? "type"
    if t == Just ("grammar" :: Text) then Right () else Left "Not a grammar element"
    case elementNodes e of
      [n] -> case n of
        NodeContent t -> pure $ Grammar {grammarText = stripDoc t}
        _ -> Left $ unwords ["A non-Content node found under 'doc' element with type grammar:", show e]
      _ -> Left $ unwords ["No or more than one child found of 'doc' element with type grammar:", show e]

newtype Doc = Doc {docText :: Text}
  deriving (Show, Eq, Generic)

instance FromElement Doc where
  fromElement = elementWithName "doc" $ \e ->
    case elementNodes e of
      [n] -> case n of
        NodeContent t -> pure $ Doc {docText = stripDoc t}
        _ -> Left $ unwords ["A non-Content node found under 'doc' element:", show e]
      _ -> Left $ unwords ["No or more than one child found of 'doc' element:", show e]

-- Not really a good instance, but good enough for the spec
instance FromAttribute Bool where
  fromAttribute t = case t of
    "1" -> pure True
    "0" -> pure False
    _ -> Left $ unwords ["Unknown Bool:", show t]

stripDoc :: Text -> Text
stripDoc = T.strip . T.unlines . map T.strip . T.lines

class FromElement a where
  fromElement :: Element -> Either String a

instance FromElement Element where
  fromElement = pure

class FromAttribute a where
  fromAttribute :: Text -> Either String a

instance FromAttribute Text where
  fromAttribute = pure

instance FromAttribute Word where
  fromAttribute t = case readMaybe (T.unpack t) of
    Nothing -> Left $ unwords ["Attribute", show t, "could not be parsed to a Word"]
    Just w -> pure w

elementWithName :: Name -> (Element -> Either String a) -> (Element -> Either String a)
elementWithName n func e =
  if elementName e == n
    then func e
    else Left $ unwords ["Expected element with name", show n, ", but got this element instead:", show e]

elementBelow :: FromElement child => Element -> Name -> Either String child
elementBelow e n = case filter ((n ==) . elementName) (elementChildrenOf e) of
  [] -> Left $ unwords ["No child with name", show n, "found under element", show e]
  [c] -> fromElement c
  _ -> Left $ unwords ["More than one child with name", show n, "found under element", show e]

maybeElementBelow :: FromElement child => Element -> Name -> Either String (Maybe child)
maybeElementBelow e n = case filter ((n ==) . elementName) (elementChildrenOf e) of
  [] -> pure Nothing
  [c] -> Just <$> fromElement c
  _ -> Left $ unwords ["More than one child with name", show n, "found under element", show e]

elementsBelow :: FromElement child => Element -> Name -> Either String [child]
elementsBelow e n = do
  mapM fromElement $ filter ((n ==) . elementName) (elementChildrenOf e)

firstElementBelow :: FromElement child => Element -> Name -> Either String (Maybe child)
firstElementBelow e n = case filter ((n ==) . elementName) (elementChildrenOf e) of
  [] -> pure Nothing
  (c : _) -> Just <$> fromElement c

firstParsingElementBelow :: FromElement child => Element -> Name -> Either String (Maybe child)
firstParsingElementBelow e n = case filter ((n ==) . elementName) (elementChildrenOf e) of
  [] -> pure Nothing
  cs -> case partitionEithers $ map fromElement cs of
    (err : _, []) -> Left err
    (_, res : _) -> pure $ Just res

-- | Parse an attribute with a given name
(.:) :: FromAttribute a => Element -> Name -> Either String a
(.:) e n = case M.lookup n (elementAttributes e) of
  Nothing -> Left $ unwords ["Attribute ", show n, "not found in element", show e]
  Just r -> fromAttribute r

-- | Parse an attribute with a given name, or Nothing if the element does not have an attribute with this name.
(.:?) :: FromAttribute a => Element -> Name -> Either String (Maybe a)
(.:?) e n = case M.lookup n (elementAttributes e) of
  Nothing -> pure Nothing
  Just r -> Just <$> fromAttribute r

elementChildrenOf :: Element -> [Element]
elementChildrenOf = mapMaybe nodeElement . elementNodes

nodeElement :: Node -> Maybe Element
nodeElement = \case
  NodeElement e -> Just e
  _ -> Nothing

nodeContent :: Node -> Maybe Text
nodeContent = \case
  NodeContent t -> Just t
  _ -> Nothing
