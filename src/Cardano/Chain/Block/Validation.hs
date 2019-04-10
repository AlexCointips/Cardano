{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE NumDecimals           #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TupleSections         #-}

module Cardano.Chain.Block.Validation
  ( updateBody
  , updateChainBlockOrBoundary
  , updateChainBoundary
  , updateHeader
  , updateBlock
  , ChainValidationState(..)
  , initialChainValidationState
  , ChainValidationError

  -- * SigningHistory
  , SigningHistory(..)
  , updateSigningHistory

  -- * UTxO
  , HeapSize(..)
  , UTxOSize(..)
  , applyBlockUTxOMVar
  , calcUTxOSize
  , foldUTxO
  , foldUTxOBlock
  , scanUTxOfromGenesis
  )
where

import Cardano.Prelude

import Control.Monad.Trans.Resource (ResIO, runResourceT)
import qualified Data.ByteString.Lazy as BSL
import Data.Coerce (coerce)
import qualified Data.Map.Strict as M
import Data.Sequence (Seq(..), (<|))
import Streaming (Of(..), Stream, hoist)
import qualified Streaming.Prelude as S

import Cardano.Chain.Block.Block
  ( ABlock(..)
  , ABlockOrBoundary(..)
  , BoundaryValidationData(..)
  , blockAttributes
  , blockDlgPayload
  , blockHashAnnotated
  , blockHeader
  , blockIssuer
  , blockLength
  , blockProof
  , blockProtocolVersion
  , blockSlot
  , blockTxPayload
  , blockUpdatePayload
  )
import Cardano.Chain.Block.Header
  ( AHeader
  , BlockSignature(..)
  , HeaderHash
  , headerAttributes
  , headerLength
  , headerSlot
  , wrapBoundaryBytes
  )
import Cardano.Chain.Block.Proof (Proof(..))
import Cardano.Chain.Common
  ( Attributes(..)
  , BlockCount(..)
  , StakeholderId
  , UnparsedFields(..)
  , mkStakeholderId
  )
import qualified Cardano.Chain.Delegation.Payload as DlgPayload
import qualified Cardano.Chain.Delegation.Validation.Activation as Activation
import qualified Cardano.Chain.Delegation.Validation.Interface as DI
import qualified Cardano.Chain.Delegation.Validation.Scheduling as Scheduling
import Cardano.Chain.Epoch.File (ParseError, mainnetEpochSlots, parseEpochFiles)
import Cardano.Chain.Genesis as Genesis
  ( Config(..)
  , GenesisWStakeholders(..)
  , GenesisHash
  , configBootStakeholders
  , configEpochSlots
  , configHeavyDelegation
  , configK
  , configProtocolMagic
  , configProtocolMagicId
  )
import Cardano.Chain.ProtocolConstants (kEpochSlots)
import Cardano.Chain.Slotting
  (EpochIndex(..), FlatSlotId(..), SlotId(..), slotNumberEpoch, unflattenSlotId)
import Cardano.Chain.Txp (ATxPayload(..), UTxO(..), genesisUtxo, recoverTxProof)
import qualified Cardano.Chain.Txp.Validation as UTxO
import qualified Cardano.Chain.Update as Update
import Cardano.Chain.Update.Validation.Endorsement (Endorsement(..))
import qualified Cardano.Chain.Update.Validation.Interface as UPI
import Cardano.Crypto
  ( ProtocolMagic
  , ProtocolMagicId
  , PublicKey
  , getProtocolMagicId
  , hashRaw
  , hashDecoded
  )

--------------------------------------------------------------------------------
-- SigningHistory
--------------------------------------------------------------------------------

-- | The history of signers in the last @K@ blocks
--
--   We maintain a map of the number of blocks signed for each stakeholder to
--   improve performance. The sum of the `BlockCount`s in the map should be
--   equal to the length of the sequence.
data SigningHistory = SigningHistory
  { shK                 :: !BlockCount
  , shSigningQueue      :: !(Seq StakeholderId)
  , shStakeholderCounts :: !(Map StakeholderId BlockCount)
  } deriving (Eq, Show, Generic, NFData)

-- | Update the `SigningHistory` with a new signer, removing the oldest value if
--   the sequence is @K@ blocks long
updateSigningHistory :: PublicKey -> SigningHistory -> SigningHistory
updateSigningHistory pk sh
  | length (shSigningQueue sh) < fromIntegral (unBlockCount $ shK sh) = sh & addStakeholderIn
  | otherwise = sh & addStakeholderIn & removeStakeholderOut
 where
  stakeholderIn = mkStakeholderId pk

  addStakeholderIn :: SigningHistory -> SigningHistory
  addStakeholderIn sh' = sh'
    { shSigningQueue      = stakeholderIn <| shSigningQueue sh'
    , shStakeholderCounts = M.adjust
      succ
      stakeholderIn
      (shStakeholderCounts sh')
    }

  removeStakeholderOut :: SigningHistory -> SigningHistory
  removeStakeholderOut sh' = case shSigningQueue sh' of
    Empty -> sh'
    rest :|> stakeholderOut -> sh'
      { shSigningQueue      = rest
      , shStakeholderCounts = M.adjust
        pred
        stakeholderOut
        (shStakeholderCounts sh')
      }


--------------------------------------------------------------------------------
-- ChainValidationState
--------------------------------------------------------------------------------

data ChainValidationState = ChainValidationState
  { cvsLastSlot        :: !FlatSlotId
  , cvsSigningHistory  :: !SigningHistory
  , cvsPreviousHash    :: !(Either GenesisHash HeaderHash)
  -- ^ GenesisHash for the previous hash of the zeroth boundary block and
  --   HeaderHash for all others.
  , cvsUtxo            :: !UTxO
  , cvsUpdateState     :: !UPI.State
  , cvsDelegationState :: !DI.State
  } deriving (Eq, Show, Generic, NFData)

-- | Create the state needed to validate the zeroth epoch of the chain. The
--   zeroth epoch starts with a boundary block where the previous hash is the
--   genesis hash.
initialChainValidationState
  :: MonadError Scheduling.Error m
  => Genesis.Config
  -> m ChainValidationState
initialChainValidationState config = do
  delegationState <- DI.initialState delegationEnv genesisDelegation
  pure $ ChainValidationState
    { cvsLastSlot       = 0
    , cvsSigningHistory = SigningHistory
      { shK = configK config
      , shStakeholderCounts = M.fromList
        . map (, BlockCount 0)
        . M.keys
        . unGenesisWStakeholders
        $ configBootStakeholders config
      , shSigningQueue = Empty
      }
    , cvsPreviousHash   = Left $ configGenesisHash config
    , cvsUtxo           = genesisUtxo config
    , cvsUpdateState    = UPI.initialState config
    , cvsDelegationState = delegationState
    }
 where
  delegationEnv = DI.Environment
    { DI.protocolMagic = configProtocolMagicId config
    , DI.allowedDelegators = M.keysSet
      . unGenesisWStakeholders
      $ configBootStakeholders config
    , DI.k           = configK config
    , DI.currentEpoch = EpochIndex 0
    , DI.currentSlot = FlatSlotId 0
    }

  genesisDelegation = configHeavyDelegation config


--------------------------------------------------------------------------------
-- ChainValidationError
--------------------------------------------------------------------------------

data ChainValidationError

  = ChainValidationBoundaryTooLarge
  -- ^ The size of an epoch boundary block exceeds the limit

  | ChainValidationBlockAttributesTooLarge
  -- ^ The size of a block's attributes is non-zero

  | ChainValidationBlockTooLarge
  -- ^ The size of a regular block exceeds the limit

  | ChainValidationHeaderAttributesTooLarge
  -- ^ The size of a block header's attributes is non-zero

  | ChainValidationHeaderTooLarge
  -- ^ The size of a block header exceeds the limit

  | ChainValidationDelegationPayloadError Text
  -- ^ There is a problem with the delegation payload signature

  | ChainValidationInvalidDelegation PublicKey PublicKey
  -- ^ The delegation used in the signature is not valid according to the ledger

  | ChainValidationGenesisHashMismatch GenesisHash GenesisHash
  -- ^ Genesis hash mismatch

  | ChainValidationExpectedGenesisHash GenesisHash HeaderHash
  -- ^ Expected GenesisHash but got HeaderHash

  | ChainValidationExpectedHeaderHash HeaderHash GenesisHash
  -- ^ Expected HeaderHash but GenesisHash

  | ChainValidationInvalidHash HeaderHash HeaderHash
  -- ^ The hash of the previous block does not match the value in the header

  | ChainValidationMissingHash HeaderHash
  -- ^ The hash of the previous block is missing and should be given hash.

  | ChainValidationUnexpectedGenesisHash HeaderHash
  -- ^ There should not be a hash of the previous but there is.

  | ChainValidationInvalidSignature BlockSignature
  -- ^ The signature of the block is invalid

  | ChainValidationDelegationProofError
  -- ^ The delegation proof does not correspond to the delegation payload

  | ChainValidationDelegationSchedulingError Scheduling.Error
  -- ^ A delegation certificate failed validation in the ledger layer

  | ChainValidationSignatureLight
  -- ^ A block is using unsupported lightweight delegation

  | ChainValidationTooManyDelegations PublicKey
  -- ^ The delegator for this block has delegated in too many recent blocks

  | ChainValidationUpdateError UPI.Error
  -- ^ Something failed to register in the update interface

  | ChainValidationUpdateProofError
  -- ^ The update payload proof did not match

  | ChainValidationUTxOValidationError UTxO.UTxOValidationError
  -- ^ A transaction failed validation in the ledger layer

  | ChainValidationUTxOProofError
  -- ^ The UTxO payload proof did not match

  deriving (Eq, Show)


--------------------------------------------------------------------------------
-- Validation Functions
--------------------------------------------------------------------------------

updateChainBlockOrBoundary
  :: MonadError ChainValidationError m
  => Genesis.Config
  -> ChainValidationState
  -> ABlockOrBoundary ByteString
  -> m ChainValidationState
updateChainBlockOrBoundary config c b = case b of
  ABOBBoundary bvd   -> updateChainBoundary c bvd
  ABOBBlock    block -> updateBlock config c block


updateChainBoundary
  :: MonadError ChainValidationError m
  => ChainValidationState
  -> BoundaryValidationData ByteString
  -> m ChainValidationState
updateChainBoundary cvs bvd = do
  case (cvsPreviousHash cvs, boundaryPrevHash bvd) of
    (Left expected, Left actual) ->
        (expected == actual)
          `orThrowError` ChainValidationGenesisHashMismatch expected actual
    (Right expected, Right actual) ->
        (expected == actual)
          `orThrowError` ChainValidationInvalidHash expected actual

    (Left gh, Right hh) ->
        throwError $ ChainValidationExpectedGenesisHash gh hh
    (Right hh, Left gh) ->
        throwError $ ChainValidationExpectedHeaderHash hh gh

  -- Validate that the block is within the size bounds
  (boundaryBlockLength bvd <= 2e6)
    `orThrowError` ChainValidationBoundaryTooLarge

  -- Update the previous hash
  pure $ cvs
    { cvsPreviousHash =
      Right
      . coerce
      . hashRaw
      . BSL.fromStrict
      . wrapBoundaryBytes
      $ boundaryHeaderBytes bvd
    }


data BodyEnvironment = BodyEnvironment
  { protocolMagic      :: !ProtocolMagic
  , k                  :: !BlockCount
  , numGenKeys         :: !Word8
  , protocolParameters :: !Update.ProtocolParameters
  , currentEpoch       :: !EpochIndex
  }

data BodyState = BodyState
  { utxo            :: !UTxO
  , updateState     :: !UPI.State
  , delegationState :: !DI.State
  }

-- | This is an implementation of the BBODY rule as per the chain specification.
--
--   Compared to `updateChain`, this does not validate any header level checks,
--   nor does it carry out anything which might be considered part of the
--   protocol.
updateBody
  :: MonadError ChainValidationError m
  => BodyEnvironment
  -> BodyState
  -> ABlock ByteString
  -> m BodyState
updateBody env bs b = do

  -- Validate the block size
  blockLength b <= maxBlockSize `orThrowError` ChainValidationBlockTooLarge

  -- Validate the block attributes size
  length attributes == 0 `orThrowError` ChainValidationBlockAttributesTooLarge

  -- Validate the delegation payload signature
  proofDelegation (blockProof b)
    == hashDecoded (blockDlgPayload b)
    `orThrowError` ChainValidationDelegationProofError

  -- Update the delegation state
  delegationState' <-
    DI.updateDelegation delegationEnv delegationState certificates
      `wrapError` ChainValidationDelegationSchedulingError

  -- Validate the transaction payload proof
  proofTxp (blockProof b)
    == recoverTxProof (blockTxPayload b)
    `orThrowError` ChainValidationUTxOProofError

  -- Update the UTxO
  utxo' <-
    UTxO.updateUTxO
        protocolMagic
        (UPI.adoptedProtocolParameters updateState)
        utxo
        txs
      `wrapError` ChainValidationUTxOValidationError

  -- Validate the update payload proof
  proofUpdate (blockProof b)
    == hashDecoded (blockUpdatePayload b)
    `orThrowError` ChainValidationUpdateProofError

  -- Update the update state
  updateState' <-
    UPI.registerUpdate updateEnv updateState updateSignal
      `wrapError` ChainValidationUpdateError

  pure $ BodyState
    { utxo        = utxo'
    , updateState = updateState'
    , delegationState = delegationState'
    }
 where
  BodyEnvironment { protocolMagic, k, numGenKeys, currentEpoch }
    = env

  BodyState { utxo, updateState, delegationState } = bs

  maxBlockSize =
    Update.ppMaxBlockSize $ UPI.adoptedProtocolParameters updateState

  UnparsedFields attributes = attrRemain $ blockAttributes b

  currentSlot = blockSlot b

  certificates = DlgPayload.getPayload $ blockDlgPayload b

  txs         = aUnTxPayload $ blockTxPayload b

  delegationEnv = DI.Environment
    { DI.protocolMagic = getProtocolMagicId protocolMagic
    , DI.allowedDelegators = M.keysSet (DI.delegationMap delegationState)
    , DI.k           = k
    , DI.currentEpoch = currentEpoch
    , DI.currentSlot = currentSlot
    }

  updateEnv = UPI.Environment
    { UPI.protocolMagic = getProtocolMagicId protocolMagic
    , UPI.k           = k
    , UPI.currentSlot = currentSlot
    , UPI.numGenKeys  = numGenKeys
    , UPI.delegationMap = DI.delegationMap delegationState
    }

  updateSignal   = UPI.Signal updateProposal updateVotes updateEndorsement

  updateProposal = Update.payloadProposal $ blockUpdatePayload b
  updateVotes    = Update.payloadVotes $ blockUpdatePayload b
  updateEndorsement =
    Endorsement (blockProtocolVersion b) (mkStakeholderId $ blockIssuer b)


data HeaderEnvironment = HeaderEnvironment
  { protocolMagic :: !ProtocolMagicId
  , k             :: !BlockCount
  , numGenKeys    :: !Word8
  , delegationMap :: !(Map StakeholderId StakeholderId)
  , lastSlot      :: !FlatSlotId
  }


-- | This is an implementation of the the BHEAD rule.
updateHeader
  :: MonadError ChainValidationError m
  => HeaderEnvironment
  -> UPI.State
  -> AHeader ByteString
  -> m UPI.State
updateHeader env st h = do
  -- Validate the header size
  headerLength h <= maxHeaderSize `orThrowError` ChainValidationHeaderTooLarge

  -- Validate the header attributes are empty
  length attributes == 0 `orThrowError` ChainValidationHeaderAttributesTooLarge

  -- Perform epoch transition
  epochTransition epochEnv st (headerSlot h)
 where
  maxHeaderSize = Update.ppMaxHeaderSize $ UPI.adoptedProtocolParameters st

  UnparsedFields attributes = attrRemain $ headerAttributes h

  HeaderEnvironment { protocolMagic, k, numGenKeys, delegationMap, lastSlot }
    = env

  epochEnv = EpochEnvironment
    { protocolMagic
    , k
    , numGenKeys
    , delegationMap
    , currentEpoch
    }

  currentEpoch = siEpoch $ unflattenSlotId (kEpochSlots k) lastSlot


data EpochEnvironment = EpochEnvironment
  { protocolMagic :: !ProtocolMagicId
  , k             :: !BlockCount
  , numGenKeys    :: !Word8
  , delegationMap :: !(Map StakeholderId StakeholderId)
  , currentEpoch  :: !EpochIndex
  }


-- | Perform epoch transition if we have moved across the epoch boundary
--
--   We pass through to the update interface UPIEC rule, which adopts any
--   confirmed proposals and cleans up the state. This corresponds to the EPOCH
--   rules from the Byron chain specification.
epochTransition
  :: MonadError ChainValidationError m
  => EpochEnvironment
  -> UPI.State
  -> FlatSlotId
  -> m UPI.State
epochTransition env st slot = if nextEpoch > currentEpoch
  then
    UPI.registerEpoch updateEnv st nextEpoch
      `wrapError` ChainValidationUpdateError
  else pure st
 where
  EpochEnvironment { protocolMagic, k, numGenKeys, delegationMap, currentEpoch }
    = env

  nextEpoch = siEpoch $ unflattenSlotId (kEpochSlots k) slot

  updateEnv = UPI.Environment
    { UPI.protocolMagic = protocolMagic
    , UPI.k           = k
    , UPI.currentSlot = slot
    , UPI.numGenKeys  = numGenKeys
    , UPI.delegationMap = delegationMap
    }


-- | This represents the CHAIN rule. It is intended more for use in tests than
--   in a real implementation, which will want to invoke its constituent rules
--   directly.
--
--   Note that this also updates the previous block hash, which would usually be
--   done as part of the PBFT rule.
updateBlock
  :: MonadError ChainValidationError m
  => Genesis.Config
  -> ChainValidationState
  -> ABlock ByteString
  -> m ChainValidationState
updateBlock config cvs b = do

  -- Update the header
  updateState' <- updateHeader headerEnv (cvsUpdateState cvs) (blockHeader b)

  let
    bodyEnv = BodyEnvironment
      { protocolMagic = configProtocolMagic config
      , k = configK config
      , numGenKeys
      , protocolParameters = UPI.adoptedProtocolParameters updateState'
      , currentEpoch = slotNumberEpoch (configEpochSlots config) (blockSlot b)
      }

    bs = BodyState
      { utxo        = cvsUtxo cvs
      , updateState = updateState'
      , delegationState = cvsDelegationState cvs
      }

  BodyState { utxo, updateState, delegationState } <- updateBody bodyEnv bs b

  pure $ cvs
    { cvsLastSlot     = blockSlot b
    , cvsPreviousHash = Right $ blockHashAnnotated b
    , cvsUtxo         = utxo
    , cvsUpdateState  = updateState
    , cvsDelegationState = delegationState
    }
 where
  headerEnv = HeaderEnvironment
    { protocolMagic = configProtocolMagicId config
    , k          = configK config
    , numGenKeys
    , delegationMap
    , lastSlot   = blockSlot b
    }

  numGenKeys :: Word8
  numGenKeys =
    case length (unGenesisWStakeholders $ configBootStakeholders config) of
      n
        | n > fromIntegral (maxBound :: Word8) -> panic
          "updateBody: Too many genesis keys"
        | otherwise -> fromIntegral n

  delegationMap =
    Activation.asDelegationMap . DI.isActivationState $ cvsDelegationState cvs


--------------------------------------------------------------------------------
-- UTxO
--------------------------------------------------------------------------------

data Error
  = ErrorParseError ParseError
  | ErrorUTxOValidationError SlotId UTxO.UTxOValidationError
  deriving (Eq, Show)

-- | Fold transaction validation over a 'Stream' of 'Block's
foldUTxO
  :: ProtocolMagic
  -> Update.ProtocolParameters
  -> UTxO
  -> Stream (Of (ABlock ByteString)) (ExceptT ParseError ResIO) ()
  -> ExceptT Error ResIO UTxO
foldUTxO pm pps utxo blocks = S.foldM_
  (foldUTxOBlock pm pps)
  (pure utxo)
  pure
  (hoist (withExceptT ErrorParseError) blocks)

-- | Fold 'updateUTxO' over the transactions in a single 'Block'
foldUTxOBlock
  :: ProtocolMagic
  -> Update.ProtocolParameters
  -> UTxO
  -> ABlock ByteString
  -> ExceptT Error ResIO UTxO
foldUTxOBlock pm pps utxo block =
  withExceptT
      (ErrorUTxOValidationError . unflattenSlotId mainnetEpochSlots $ blockSlot
        block
      )
    $ UTxO.updateUTxO pm pps utxo (aUnTxPayload $ blockTxPayload block)

-- | Size of a heap value, in words
newtype HeapSize a =
  HeapSize { unHeapSize :: Int}
  deriving Show

-- | Number of entries in the UTxO
newtype UTxOSize =
  UTxOSize { unUTxOSize :: Int}
  deriving Show

-- | Apply a block of 'Tx's to the 'UTxO' and update
-- an MVar with the heap size and map size of the 'UTxO'
-- along with the 'SlotId'.
applyBlockUTxOMVar
  :: ProtocolMagic
  -> Update.ProtocolParameters
  -> MVar (HeapSize UTxO, UTxOSize, SlotId)
  -> UTxO
  -> ABlock ByteString
  -> ExceptT Error ResIO UTxO
applyBlockUTxOMVar pm pps sizeMVar utxo block = do
  resResult <- liftIO . runResourceT . runExceptT $ foldUTxOBlock
    pm
    pps
    utxo
    block
  case resResult of
    Left  e           -> throwE e
    Right updatedUtxo -> do
      _ <- liftIO $ takeMVar sizeMVar
      liftIO . putMVar sizeMVar $ calcUTxOSize block updatedUtxo
      return updatedUtxo

-- | Return the updated 'UTxO' after applying a block.
scanUTxOfromGenesis
  :: ProtocolMagic
  -> Update.ProtocolParameters
  -> UTxO
  -> MVar (HeapSize UTxO, UTxOSize, SlotId)
  -> [FilePath]
  -> IO (Either Error UTxO)
scanUTxOfromGenesis pm pps utxo sizeMVar fs =
  runResourceT
    . runExceptT
    $ S.foldM_ (applyBlockUTxOMVar pm pps sizeMVar) (pure utxo) pure
    $ hoist (withExceptT ErrorParseError) blocks
  where blocks = parseEpochFiles mainnetEpochSlots fs

calcUTxOSize :: ABlock ByteString -> UTxO -> (HeapSize UTxO, UTxOSize, SlotId)
calcUTxOSize blk utxo =
  ( HeapSize . heapWords $ unUTxO utxo
  , UTxOSize . M.size $ unUTxO utxo
  , unflattenSlotId mainnetEpochSlots $ blockSlot blk
  )
