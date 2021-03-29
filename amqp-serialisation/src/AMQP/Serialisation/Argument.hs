{-# LANGUAGE TypeSynonymInstances #-}

module AMQP.Serialisation.Argument where

import AMQP.Serialisation.Base
import AMQP.Serialisation.Generated
import Data.Attoparsec.ByteString as Parse
import Data.Proxy
import Data.Validity.ByteString ()
import Data.Validity.Containers ()

class Method a where
  methodClassId :: Proxy a -> ClassId
  methodMethodId :: Proxy a -> MethodId
  buildMethodArguments :: a -> [Argument]
  parseMethodArguments :: Parser a

class IsArgument a where
  toArgument :: a -> Argument
  parseArgument :: Parser a

instance IsArgument Bit where
  toArgument = ArgumentBit
  parseArgument = parseBit

instance IsArgument Octet where
  toArgument = ArgumentOctet
  parseArgument = parseOctet

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
