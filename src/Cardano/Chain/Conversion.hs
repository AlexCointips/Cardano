{-# LANGUAGE OverloadedStrings #-}

module Cardano.Chain.Conversion
  ( SpecifyGenesis(..)
  , convertConfig
  )
where

import Cardano.Prelude

import           Control.Monad.Trans.Either (EitherT, firstEitherT, hoistEither)
import qualified Data.Map.Strict as M
import           Data.Time (UTCTime)
import           Formatting (build, sformat)
import           Formatting.Buildable(Buildable)
import           Text.Megaparsec
import           Text.Megaparsec.Char (string)

import           Cardano.Binary (Annotated(..), Raw)
import           Cardano.Chain.Common (BlockCount(..), TxFeePolicy(..), lovelacePortionFromDouble, mkLovelace, mkLovelacePortion)
import qualified Cardano.Chain.Common
import           Cardano.Chain.Genesis.AvvmBalances (GenesisAvvmBalances(..))
import           Cardano.Chain.Genesis.Config (Config, ConfigurationError(..), StaticConfig(..), mkConfigFromFile, mkConfigFromStaticConfig)
import           Cardano.Chain.Genesis.Delegation (GenesisDelegation(..))
import           Cardano.Chain.Genesis.Initializer (GenesisInitializer(..), FakeAvvmOptions(..), TestnetBalanceOptions(..))
import           Cardano.Chain.Genesis.Spec (mkGenesisSpec)
import           Cardano.Chain.Update (ProtocolParameters(..), SoftforkRule(..))
import           Cardano.Chain.Slotting (EpochIndex(..), FlatSlotId(..))
import           Cardano.Crypto.Hashing (Hash, decodeAbstractHash)
import           Cardano.Crypto.ProtocolMagic (AProtocolMagic(..), ProtocolMagicId(..), RequiresNetworkMagic(..))
import           Cardano.Shell.Constants.Types as Shell


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

-- | Converts 'CardanoConfiguration' (from 'cardano-shell') into a `Config` (from 'cardano-ledger')
convertConfig :: SpecifyGenesis -> Shell.CardanoConfiguration -> EitherT ConfigurationError IO Config
convertConfig swtch cc =
    case swtch of
      FromSrc -> do
                   -- Genesis hash
                   let mainnetGenFp = geSrc . coGenesis $ ccCore cc
                   gHash <- hoistEither $ decodeGenesisHash genesisHash

                   -- RequiresNetworkMagic
                   reqNM <- hoistEither reqNetworkMagic

                   mkConfigFromFile reqNM mainnetGenFp gHash
      FromSpec _ _ -> do

        -- GenesisAvvmBalances
        avvmBal <- hoistEither genAvvmBal

        -- GenesisDelegation
        gDeleg <- hoistEither genDeleg

        -- ProtocolParameters
        let pParams = pProtocolParameters (spBlockVersionData shellSpec)

        -- BlockCount
        let blkCount = BlockCount . prK $ spProtocolConstants shellSpec

        -- Protocol Magic
        reqNM <- hoistEither reqNetworkMagic
        let pmId =  prProtocolMagic $ spProtocolConstants shellSpec
        let pm = AProtocolMagic (Annotated (ProtocolMagicId pmId) ()) reqNM

         -- GenesisInitializer
        let init = spInitializer shellSpec

        -- TestNetBalance
        let shellTestNetBal = inTestBalance init
        rMenShare <- firstEitherT ConfigPortionConvErr $ hoistEither (lovelacePortionFromDouble $ teRichmenShare shellTestNetBal)
        tBalance <-  firstEitherT ConfigLovelaceConvErr $ hoistEither (mkLovelace $ teTotalBalance shellTestNetBal)
        let testNetBalanceOpts = TestnetBalanceOptions (tePoors shellTestNetBal) (teRichmen shellTestNetBal) tBalance rMenShare (teUseHDAddresses shellTestNetBal)

        -- FakeAvvmBalance
        let shellFakeAvvm = inFakeAvvmBalance init
        faoOneBal <- firstEitherT ConfigLovelaceConvErr $ hoistEither (mkLovelace $ faOneBalance shellFakeAvvm)
        let fakeAvvmBal = FakeAvvmOptions (faCount shellFakeAvvm) faoOneBal

        -- AvvmBalanceFactor
        avvmBalFact <- firstEitherT ConfigPortionConvErr $ hoistEither (mkLovelacePortion . fromIntegral $  inAVVMBalanceFactor init)

        --UseHeavyDlg
        let hvyDlg = inUseHeavyDlg init

        --Seed
        let seed = fromIntegral $ inSeed init :: Integer

        let genesisInitializer = GenesisInitializer testNetBalanceOpts fakeAvvmBal avvmBalFact hvyDlg seed

        -- GenesisSpec
        gSpec <- firstEitherT ConfigGenSpecConvErr . hoistEither $ mkGenesisSpec avvmBal gDeleg pParams blkCount pm genesisInitializer

        mkConfigFromStaticConfig reqNM Nothing Nothing (GCSpec gSpec)

  where
    decodeGenesisHash :: Text -> Either ConfigurationError (Hash Raw)
    decodeGenesisHash genHash = first GenesisHashDecodeError $ decodeAbstractHash genHash
    genAvvmBal :: Either ConfigurationError GenesisAvvmBalances
    genAvvmBal = first ConfigParsingError $ runParser pGenesisAvvmBalances "Cardano.Chain.Conversion" (spAVVMDistr shellSpec)
    genDeleg :: Either ConfigurationError GenesisDelegation
    genDeleg = first ConfigParsingError $ runParser pGenesisDelegation "Cardano.Chain.Conversion" (spHeavyDelegation shellSpec)
    genesisHash :: Text
    genesisHash = geFileHash . coGenesis $ ccCore cc
    reqNetworkMagic ::  Either ConfigurationError RequiresNetworkMagic
    reqNetworkMagic = first ConfigParsingError $ runParser pRequiresNetworkMagic "Cardano.Chain.Conversion" (coRequiresNetworkMagic $ ccCore cc)
    shellSpec :: Shell.Spec
    shellSpec = geSpec . coGenesis $ ccCore cc



--------------------------------------------------------------------------------
-- Parsers
--------------------------------------------------------------------------------

pRequiresNetworkMagic :: Parsec Void Text RequiresNetworkMagic
pRequiresNetworkMagic = choice [ RequiresNoMagic <$ string "RequiresNoMagic"
                               , RequiresMagic <$ string "RequiresMagic"
                               ]

pGenesisAvvmBalances :: Parsec Void Text GenesisAvvmBalances
pGenesisAvvmBalances = return $ GenesisAvvmBalances M.empty

pGenesisDelegation :: Parsec Void Text GenesisDelegation
pGenesisDelegation = return $ UnsafeGenesisDelegation M.empty

pProtocolParameters :: Shell.BlockVersionData -> ProtocolParameters
pProtocolParameters bvd =
   ProtocolParameters
     (bvdScriptVersion bvd)
     (toEnum $ bvdSlotDuration bvd)
     (bvdMaxBlockSize bvd)
     (bvdMaxHeaderSize bvd)
     (bvdMaxTxSize bvd)
     (bvdMaxProposalSize bvd)
     (retrieveConfigVal mkLovelacePortion $ bvdMpcThd bvd)
     (retrieveConfigVal mkLovelacePortion $ bvdHeavyDelThd bvd)
     (retrieveConfigVal mkLovelacePortion $ bvdUpdateVoteThd bvd)
     (retrieveConfigVal mkLovelacePortion $ bvdUpdateProposalThd bvd)
     (FlatSlotId $ bvdUpdateImplicit bvd)
     ppSoftForkRule
     ppTxfeePolicy
     (EpochIndex $ bvdUnlockStakeEpoch bvd)
  where
    bvdSoftForkRule :: Shell.SoftForkRule
    bvdSoftForkRule = bvdSoftforkRule bvd

    bvdTxFeePolicy :: Shell.TxSizeLinear
    bvdTxFeePolicy = txfTXSizeLinear $ bvdTXFeePolicy bvd

    ppSoftForkRule :: SoftforkRule
    ppSoftForkRule =
      SoftforkRule
       (retrieveConfigVal mkLovelacePortion $ sfrInitThd bvdSoftForkRule)
       (retrieveConfigVal mkLovelacePortion $ sfrMinThd bvdSoftForkRule)
       (retrieveConfigVal mkLovelacePortion $ sfrThdDecrement bvdSoftForkRule)

    ppTxfeePolicy :: Cardano.Chain.Common.TxFeePolicy
    ppTxfeePolicy = TxFeePolicyTxSizeLinear
       $ Cardano.Chain.Common.TxSizeLinear
          (retrieveConfigVal mkLovelace (txsA bvdTxFeePolicy))
          (retrieveConfigVal mkLovelace (txsB bvdTxFeePolicy))

--------------------------------------------------------------------------------
-- Helper functions
--------------------------------------------------------------------------------

-- | Converts values from 'Shell.CardanoConfiguration' in to the required types for 'cardano-ledger'
retrieveConfigVal :: Buildable e => (a -> Either e b) -> a -> b
retrieveConfigVal converter val = either (panic . sformat build) identity $ converter val
