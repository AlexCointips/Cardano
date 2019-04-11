{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Cardano.Chain.Txp.Validation
  ( tests
  , getTheUTxO
  )
where

import Cardano.Prelude

import Control.Monad.Trans.Resource (runResourceT)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.String (fromString)
import System.FilePath (takeFileName)

import Hedgehog
  ( Group(..)
  , Property
  , PropertyName
  , checkSequential
  , evalEither
  , property
  , withTests
  )

import Cardano.Chain.Update (ProtocolParameters(..))
import Cardano.Chain.Block (foldUTxO)
import Cardano.Chain.Epoch.File (parseEpochFile)
import Cardano.Chain.Genesis (configProtocolMagic, configEpochSlots)
import Cardano.Chain.Slotting (EpochSlots)
import Cardano.Chain.Txp (UTxO, genesisUtxo)
import Cardano.Crypto (ProtocolMagic(..))
import Cardano.Mirror (mainnetEpochFiles)

import Test.Cardano.Chain.Config (readMainetCfg)
import Test.Options (TestScenario(..))
import Test.Cardano.Chain.Genesis.Dummy (dummyProtocolParameters)


getTheUTxO :: IO UTxO
getTheUTxO = do
  let scenario = ContinuousIntegration

  -- Get @Genesis.Config@ from the mainnet JSON configuration
  genesisConfig <- readMainetCfg

  -- Extract mainnet 'ProtocolMagic'
  let pm = configProtocolMagic genesisConfig

  -- Create an 'IORef' containing the genesis 'UTxO'
  utxoRef <- newIORef $ genesisUtxo genesisConfig

  let
    takeFiles :: [FilePath] -> [FilePath]
    takeFiles = case scenario of
      ContinuousIntegration -> identity
      Development           -> take 15
      QualityAssurance      -> identity

  -- Get a list of epoch files to perform validation on
  files <- takeFiles <$> mainnetEpochFiles

  -- Validate the transactions of each epoch file in a single 'Property' and
  -- check them all sequentially
  let
    es = configEpochSlots genesisConfig

    properties :: [(PropertyName, Property)]
    properties = zip (fromString . takeFileName <$> files) (epochValid pm es utxoRef <$> files)
  checkSequential $ Group "Test.Cardano.Chain.Txp.Validation" properties
  readIORef utxoRef

-- | These tests perform transaction validation over mainnet epoch files
--
--   We have chosen to split each epoch file into its own 'Property', because
--   this leads to a clearer log of progress during testing. This requires an
--   'IORef' to synchronise the 'UTxO' between epochs, as 'Property's do not
--   return values.
tests :: TestScenario -> IO Bool
tests scenario = do

  -- Get @Genesis.Config@ from the mainnet JSON configuration
  genesisConfig <- readMainetCfg

  -- Extract mainnet 'ProtocolMagic'
  let pm = configProtocolMagic genesisConfig

  -- Create an 'IORef' containing the genesis 'UTxO'
  utxoRef <- newIORef $ genesisUtxo genesisConfig

  let
    takeFiles :: [FilePath] -> [FilePath]
    takeFiles = case scenario of
      ContinuousIntegration -> identity
      Development           -> take 15
      QualityAssurance      -> identity

  -- Get a list of epoch files to perform validation on
  files <- takeFiles <$> mainnetEpochFiles

  -- Validate the transactions of each epoch file in a single 'Property' and
  -- check them all sequentially
  let
    es = configEpochSlots genesisConfig

    properties :: [(PropertyName, Property)]
    properties = zip (fromString . takeFileName <$> files) (epochValid pm es utxoRef <$> files)
  checkSequential $ Group "Test.Cardano.Chain.Txp.Validation" properties

-- | Check that a single epoch's transactions are valid by folding over 'Block's
epochValid :: ProtocolMagic -> EpochSlots -> IORef UTxO -> FilePath -> Property
epochValid pm es utxoRef fp = withTests 1 . property $ do
  utxo <- liftIO $ readIORef utxoRef
  let stream = parseEpochFile es fp
  result  <- (liftIO . runResourceT . runExceptT) (foldUTxO pm pparams utxo stream)
  newUtxo <- evalEither result
  liftIO $ writeIORef utxoRef newUtxo


pparams :: ProtocolParameters
pparams = dummyProtocolParameters
  -- { ppScriptVersion    = 0
  -- , ppSlotDuration     = 7000
  -- , ppMaxBlockSize     = 2000000
  -- , ppMaxHeaderSize    = 2000000
  { ppMaxTxSize        = 65000
  }
  -- , ppMaxProposalSize  = 700
  -- , ppMpcThd           = mkKnownLovelacePortion @10000000000000
  -- , ppHeavyDelThd      = mkKnownLovelacePortion @5000000000000
  -- , ppUpdateVoteThd    = mkKnownLovelacePortion @1000000000000
  -- , ppUpdateProposalThd = mkKnownLovelacePortion @100000000000000
  -- , ppUpdateImplicit   = 10
  -- , ppSoftforkRule     = SoftforkRule
  --   (mkKnownLovelacePortion @900000000000000)
  --   (mkKnownLovelacePortion @600000000000000)
  --   (mkKnownLovelacePortion @50000000000000)
  -- , ppTxFeePolicy      = TxFeePolicyTxSizeLinear
  --   (TxSizeLinear (mkKnownLovelace @155381) (mkKnownLovelace @44))
  -- , ppUnlockStakeEpoch = EpochIndex maxBound
  -- }
