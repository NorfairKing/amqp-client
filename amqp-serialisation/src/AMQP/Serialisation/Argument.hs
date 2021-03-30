{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}

module AMQP.Serialisation.Argument where

import AMQP.Serialisation.Base
import AMQP.Serialisation.Generated.DomainTypes
import Data.Attoparsec.ByteString as Parse
import Data.Proxy
import Data.Validity.ByteString ()
import Data.Validity.Containers ()
import GHC.Generics

class IsMethod a where
  methodClassId :: Proxy a -> ClassId
  methodMethodId :: Proxy a -> MethodId
  methodSynchronous :: Proxy a -> Bool
  buildMethodArguments :: a -> [Argument]
  default buildMethodArguments :: (Generic a, GIsMethod (Rep a)) => a -> [Argument]
  buildMethodArguments = gBuildArguments . from
  parseMethodArguments :: Parser a
  default parseMethodArguments :: (Generic a, GIsMethod (Rep a)) => Parser a
  parseMethodArguments = to <$> gParseArguments

class IsArgument a where
  toArgument :: a -> Argument
  parseArgument :: Parser a

instance IsArgument Bit where
  toArgument = ArgumentBit
  parseArgument = parseBit

instance IsArgument Octet where
  toArgument = ArgumentOctet
  parseArgument = parseOctet

instance IsArgument ShortUInt where
  toArgument = ArgumentShortUInt
  parseArgument = parseShortUInt

instance IsArgument LongUInt where
  toArgument = ArgumentLongUInt
  parseArgument = parseLongUInt

instance IsArgument LongLongUInt where
  toArgument = ArgumentLongLongUInt
  parseArgument = parseLongLongUInt

instance IsArgument ShortString where
  toArgument = ArgumentShortString
  parseArgument = parseShortString

instance IsArgument LongString where
  toArgument = ArgumentLongString
  parseArgument = parseLongString

instance IsArgument Timestamp where
  toArgument = ArgumentTimestamp
  parseArgument = parseTimestamp

instance IsArgument FieldTable where
  toArgument = ArgumentFieldTable
  parseArgument = parseFieldTable

-- Here comes the Generic deriving of 'Method'
--
-- It saves us from generating the implementation of instances of 'Method' for
-- all the generated types.
--
-- All the instances would look like this:
--
-- > instance Method OurMethod where
-- >   methodClassId Proxy = 42
-- >   methodMethodId Proxy = 43
-- >   buildMethodArguments a = [
-- >       toArgument field1,
-- >       toArgument field2,
-- >       toArgument field3
-- >     ]
-- >   parseMethodArguments =
-- >     OurMethod
-- >       <$> parseArgument
-- >       <*> parseArgument
-- >       <*> parseArgument
--
-- So we try to derive them using Generics instead of generating all that.
--
-- Note that this _does_ mean that the fields need to be in the right order.
class GIsMethod f where
  gBuildArguments :: f a -> [Argument]
  gParseArguments :: Parser (f a)

-- No instance for the Void constructor.

-- | Constructor without arguments
instance GIsMethod U1 where
  gBuildArguments _ = [] -- No arguments
  gParseArguments = pure U1

-- | Constructor for product types
instance (GIsMethod a, GIsMethod b) => GIsMethod (a :*: b) where
  -- These are small lists anyway.
  gBuildArguments (a :*: b) = gBuildArguments a ++ gBuildArguments b
  gParseArguments = (:*:) <$> gParseArguments <*> gParseArguments

-- | Constructor for Meta-info that we don't need: constructor names, etc
instance GIsMethod a => GIsMethod (M1 i c a) where
  gBuildArguments = gBuildArguments . unM1
  gParseArguments = M1 <$> gParseArguments

-- | Constructor for the leaves: Where we get the arguments
instance IsArgument a => GIsMethod (K1 R a) where
  gBuildArguments (K1 a) = [toArgument a]
  gParseArguments = K1 <$> parseArgument
