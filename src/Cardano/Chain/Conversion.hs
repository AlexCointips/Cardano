{-# LANGUAGE OverloadedStrings #-}

module Cardano.Chain.Conversion
  ( SpecifyGenesis(..)
  , convertConfig
  )
where

import Cardano.Prelude

import Control.Monad.Trans.Either (EitherT, hoistEither, firstEitherT, runEitherT)
import Data.Time (UTCTime)

import Cardano.Binary (Raw)
import Cardano.Chain.Common (parseReqNetworkMag)
import Cardano.Chain.Genesis.Config (Config, ConfigurationError(..), mkConfigFromFile)
import Cardano.Crypto.Hashing (Hash, decodeAbstractHash)
import Cardano.Shell.Constants.Types as Shell

-- | This module converts `CardanoConfiguration` to `Genesis.Config`

-- | Data type that lets us specify genesis from a file (`FromSrc`)
-- or from our own specification (`FromSpec`).
data SpecifyGenesis
  = FromSrc
  -- ^ Convert configuration based on `mainnet-genesis.json`
  | FromSpec (Maybe UTCTime) (Maybe Integer)
  -- ^ Convert configuration based on 'geSpec'/`GCSpec`
  --   If we provide a seed ('Maybe Integer') this will override
  --   the seed in `GenesisSpec` and we will also need to provide
  --   a start time (Maybe UTCTime)

convertConfig
  :: SpecifyGenesis
  -> Shell.CardanoConfiguration
  -> EitherT ConfigurationError IO Config
convertConfig spGen cc = case spGen of
  FromSrc -> do
    let mainnetGenFp = geSrc . coGenesis $ ccCore cc
    gHash <- liftIO . runEitherT $ decodeGenesisHash genesisHash
    mkConfigFromFile reqNetworkMagic mainnetGenFp gHash

  FromSpec _ _ -> panic "Placeholder"
 where
  reqNetworkMagic = parseReqNetworkMag . coRequiresNetworkMagic $ ccCore cc
  genesisHash :: Text
  genesisHash = geFileHash . coGenesis $ ccCore cc
  decodeGenesisHash :: Text -> EitherT ConfigurationError IO (Hash Raw)
  decodeGenesisHash genHash =
    firstEitherT GenesisHashDecodeError . hoistEither $ decodeAbstractHash
      genHash
