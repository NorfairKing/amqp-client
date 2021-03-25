{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module AMQP.Client where

import AMQP.Serialisation
import qualified Data.Attoparsec as Attoparsec
import Data.ByteString (ByteString)
import qualified Data.ByteString as SB
import Data.ByteString.Builder as ByteString (Builder)
import qualified Data.ByteString.Builder as SBB
import qualified Data.ByteString.Lazy as LB
import GHC.Generics (Generic)
import qualified Network.Connection as Network
import qualified Network.Socket as Socket
import UnliftIO

data ConnectionSettings = ConnectionSettings
  { connectionSettingHostName :: Socket.HostName,
    connectionSettingPort :: Socket.PortNumber
  }
  deriving (Show, Eq, Generic)

data Connection = Connection
  { connectionNetworkConnection :: !Network.Connection
  }
  deriving (Generic)

-- | Set up (and clean up) a connection to the AMQP server.
--
-- If you don't know what 'MonadUnliftIO' is, you can just pretent it is 'IO'.
--
-- We do not expose an 'openConnection' and 'closeConnection' function just so
-- you can't get the 'bracket' wrong.
withConnection :: MonadUnliftIO m => ConnectionSettings -> (Connection -> m a) -> m a
withConnection ConnectionSettings {..} callback = do
  connectionContext <- liftIO Network.initConnectionContext
  let connectionParams =
        Network.ConnectionParams
          { Network.connectionHostname = connectionSettingHostName,
            Network.connectionPort = connectionSettingPort,
            Network.connectionUseSecure = Nothing,
            Network.connectionUseSocks = Nothing
          }
  bracket
    (liftIO $ Network.connectTo connectionContext connectionParams)
    (liftIO . Network.connectionClose)
    $ \networkConnection -> do
      connectionPutBuilder networkConnection protocolHeader
      frame <- connectionParse networkConnection parseFrame
      liftIO $ print (frame :: Either String Frame)
      let amqpConnection = Connection {connectionNetworkConnection = networkConnection}
      callback amqpConnection

connectionPutBuilder :: MonadIO m => Network.Connection -> ByteString.Builder -> m ()
connectionPutBuilder conn b = liftIO $ mapM_ (Network.connectionPut conn) (LB.toChunks (SBB.toLazyByteString b))

-- TODO keep track of the last bit of bytestring that we already got.
connectionParse :: MonadIO m => Network.Connection -> Attoparsec.Parser a -> m (Either String a)
connectionParse conn parser = liftIO $ Attoparsec.eitherResult <$> Attoparsec.parseWith (Network.connectionGet conn chunkSize) parser SB.empty

-- | How many bytes to read at a time, at most
chunkSize :: Int
chunkSize = 4098
