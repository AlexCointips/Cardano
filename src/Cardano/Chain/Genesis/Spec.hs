{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}

module Cardano.Chain.Genesis.Spec
  ( GenesisSpec(..)
  , mkGenesisSpec
  )
where

import Cardano.Prelude
import Prelude (String)

import Data.Aeson.Options (defaultOptions)
import Data.Aeson.TH (deriveJSON)
import Data.List (nub)
import qualified Data.Map.Strict as M

import Cardano.Chain.Common (BlockCount)
import Cardano.Crypto (ProtocolMagic)
import Cardano.Chain.Genesis.AvvmBalances (GenesisAvvmBalances(..))
import Cardano.Chain.Genesis.Delegation (GenesisDelegation(..))
import Cardano.Chain.Genesis.Initializer (GenesisInitializer(..))
import Cardano.Chain.Update.ProtocolParameters (ProtocolParameters)


-- | Specification how to generate full 'GenesisData'
data GenesisSpec = UnsafeGenesisSpec
  { gsAvvmDistr          :: !GenesisAvvmBalances
  -- ^ Genesis data describes avvm utxo
  , gsHeavyDelegation    :: !GenesisDelegation
  -- ^ Genesis state of heavyweight delegation. Will be concatenated with
  --   delegation genesis keyHashes if 'tiUseHeavyDlg' is 'True'
  , gsProtocolParameters :: !ProtocolParameters
  -- ^ Genesis 'ProtocolParameters'
  , gsK                  :: !BlockCount
  -- ^ The security parameter of the Ouroboros protocol
  , gsProtocolMagic      :: !ProtocolMagic
  -- ^ The magic number unique to any instance of Cardano
  , gsInitializer        :: !GenesisInitializer
  -- ^ Other data which depend on genesis type
  } deriving (Eq, Show, Generic)

deriveJSON defaultOptions ''GenesisSpec

-- | Safe constructor for 'GenesisSpec'. Throws error if something
-- goes wrong.
mkGenesisSpec
  :: GenesisAvvmBalances
  -> GenesisDelegation
  -> ProtocolParameters
  -> BlockCount
  -> ProtocolMagic
  -> GenesisInitializer
  -> Either String GenesisSpec
mkGenesisSpec avvmDistr delega bvd k pm specType = do
  let avvmKeys = M.keys $ unGenesisAvvmBalances avvmDistr
  (length (nub avvmKeys) == length avvmKeys)
    `orThrowError` "mkGenesisSpec: there are duplicates in avvm balances"
  -- All checks passed
  pure $ UnsafeGenesisSpec avvmDistr delega bvd k pm specType
