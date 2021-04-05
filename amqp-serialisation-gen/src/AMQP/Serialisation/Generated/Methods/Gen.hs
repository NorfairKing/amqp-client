{-# OPTIONS_GHC -fno-warn-orphans #-}

module AMQP.Serialisation.Generated.Methods.Gen where

import AMQP.Serialisation.Base.Gen ()
import AMQP.Serialisation.Generated.Methods
import Data.GenValidity

instance GenValid ConnectionStart where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid ConnectionStartOk where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid ConnectionSecure where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid ConnectionSecureOk where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid ConnectionTune where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid ConnectionTuneOk where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid ConnectionOpen where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid ConnectionOpenOk where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid ConnectionClose where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid ConnectionCloseOk where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid ConnectionBlocked where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid ConnectionUnblocked where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid ConnectionUpdateSecret where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid ConnectionUpdateSecretOk where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid ChannelOpen where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid ChannelOpenOk where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid ChannelFlow where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid ChannelFlowOk where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid ChannelClose where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid ChannelCloseOk where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid ExchangeDeclare where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid ExchangeDeclareOk where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid ExchangeDelete where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid ExchangeDeleteOk where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid ExchangeBind where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid ExchangeBindOk where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid ExchangeUnbind where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid ExchangeUnbindOk where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid QueueDeclare where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid QueueDeclareOk where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid QueueBind where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid QueueBindOk where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid QueueUnbind where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid QueueUnbindOk where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid QueuePurge where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid QueuePurgeOk where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid QueueDelete where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid QueueDeleteOk where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid BasicQos where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid BasicQosOk where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid BasicConsume where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid BasicConsumeOk where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid BasicCancel where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid BasicCancelOk where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid BasicPublish where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid BasicReturn where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid BasicDeliver where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid BasicGet where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid BasicGetOk where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid BasicGetEmpty where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid BasicAck where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid BasicReject where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid BasicRecoverAsync where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid BasicRecover where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid BasicRecoverOk where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid BasicNack where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid TxSelect where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid TxSelectOk where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid TxCommit where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid TxCommitOk where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid TxRollback where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid TxRollbackOk where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid ConfirmSelect where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid ConfirmSelectOk where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering
