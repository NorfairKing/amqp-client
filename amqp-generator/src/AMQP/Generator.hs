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

parseConstant :: Element -> Maybe Constant
parseConstant e = do
  guard $ elementName e == "constant"
  constantName <- M.lookup "name" $ elementAttributes e
  constantValueText <- M.lookup "value" $ elementAttributes e
  constantValue <- readMaybe (T.unpack constantValueText)
  constantClass <- M.lookup "class" $ elementAttributes e
  let constantDoc = parseConstantDoc e
  pure Constant {..}

parseConstantDoc :: Element -> Maybe Text
parseConstantDoc =
  listToMaybe
    . mapMaybe nodeContent
    . concatMap elementNodes
    . filter ((== "doc") . elementName)
    . mapMaybe nodeElement
    . elementNodes

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
