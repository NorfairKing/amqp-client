{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module AMQP.Serialisation.Argument where

import AMQP.Serialisation.Base
import AMQP.Serialisation.Generated.DomainTypes
import Data.Attoparsec.ByteString as Parse
import Data.Proxy
import Data.Validity.ByteString ()
import Data.Validity.Containers ()
import GHC.Generics

class SynchronousRequest a where
  type SynchronousResponse a :: *

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

class IsMethod a where
  methodClassId :: Proxy a -> ClassId
  methodMethodId :: Proxy a -> MethodId
  methodSynchronous :: Proxy a -> Bool
  parseMethodArguments :: Parser a
  buildMethodArguments :: a -> [Argument]
  default buildMethodArguments :: (Generic a, GIsMethod (Rep a)) => a -> [Argument]
  buildMethodArguments = gBuildMethodArguments . from

-- Here comes the Generic deriving of 'buildMethodArguments'
--
-- It saves us from generating the implementation of instances of
-- 'buildMethodArguments' for all the generated method types.
--
-- All the instances would look like this:
--
-- >   buildMethodArguments a = [
-- >       toArgument field1,
-- >       toArgument field2,
-- >       toArgument field3
-- >     ]
--
-- So we try to derive them using Generics instead of generating all that.
--
-- Note that this _does_ mean that the fields need to be in the right order.
class GIsMethod f where
  gBuildMethodArguments :: f a -> [Argument]

-- No instance for the Void constructor.

-- | Constructor without arguments
instance GIsMethod U1 where
  gBuildMethodArguments _ = [] -- No arguments

-- | Constructor for product types
instance (GIsMethod a, GIsMethod b) => GIsMethod (a :*: b) where
  -- These are small lists anyway.
  gBuildMethodArguments (a :*: b) = gBuildMethodArguments a ++ gBuildMethodArguments b

-- | Constructor for Meta-info that we don't need: constructor names, etc
instance GIsMethod a => GIsMethod (M1 i c a) where
  gBuildMethodArguments = gBuildMethodArguments . unM1

-- | Constructor for the leaves: Where we get the arguments
instance IsArgument a => GIsMethod (K1 R a) where
  gBuildMethodArguments (K1 a) = [toArgument a]

class IsContentHeader a where
  contentHeaderClassId :: Proxy a -> ClassId
  buildContentHeaderArguments :: a -> [Maybe Argument]
  default buildContentHeaderArguments :: (Generic a, GIsContentHeader (Rep a)) => a -> [Maybe Argument]
  buildContentHeaderArguments = gBuildContentHeaderArguments . from

-- Here comes the Generic deriving of 'buildContenHeaderArguments'
--
-- It saves us from generating the implementation of instances of
-- 'buildContentHeaderArguments' for all the generated content header types.
--
-- All the instances would look like this:
--
-- >   buildContentHeaderArguments a = [
-- >       toArgument <$> field1,
-- >       toArgument <$> field2,
-- >       toArgument <$> field3
-- >     ]
--
-- So we try to derive them using Generics instead of generating all that.
--
-- Note that this _does_ mean that the fields need to be in the right order.
class GIsContentHeader f where
  gBuildContentHeaderArguments :: f a -> [Maybe Argument]

-- No instance for the Void constructor.

-- | Constructor without arguments
instance GIsContentHeader U1 where
  gBuildContentHeaderArguments _ = [] -- No arguments

-- | Constructor for product types
instance (GIsContentHeader a, GIsContentHeader b) => GIsContentHeader (a :*: b) where
  -- These are small lists anyway.
  gBuildContentHeaderArguments (a :*: b) = gBuildContentHeaderArguments a ++ gBuildContentHeaderArguments b

-- | Constructor for Meta-info that we don't need: constructor names, etc
instance GIsContentHeader a => GIsContentHeader (M1 i c a) where
  gBuildContentHeaderArguments = gBuildContentHeaderArguments . unM1

-- | Constructor for the leaves: Where we get the arguments
instance IsArgument a => GIsContentHeader (K1 R (Maybe a)) where
  gBuildContentHeaderArguments (K1 ma) = [toArgument <$> ma]
