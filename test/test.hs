{-# LANGUAGE NamedFieldPuns #-}

module Main
  ( main
  )
where

import Cardano.Prelude

import Hedgehog (Group(..))
import Hedgehog.Internal.Property (GroupName(..), PropertyName(..))
import Test.Tasty (TestTree, askOption, testGroup)
import Test.Tasty.Hedgehog (testProperty)

import Test.Options (TSGroup, mainWithTestScenario)

import qualified Test.Cardano.Chain.Block.CBOR
import qualified Test.Cardano.Chain.Block.Validation
import qualified Test.Cardano.Chain.Common.Address
import qualified Test.Cardano.Chain.Common.CBOR
import qualified Test.Cardano.Chain.Common.Compact
import qualified Test.Cardano.Chain.Common.Json
import qualified Test.Cardano.Chain.Common.Lovelace
import qualified Test.Cardano.Chain.Delegation.CBOR
import qualified Test.Cardano.Chain.Delegation.Model
import qualified Test.Cardano.Chain.Epoch.File
import qualified Test.Cardano.Chain.Genesis.Json
import qualified Test.Cardano.Chain.Slotting.CBOR
import qualified Test.Cardano.Chain.Slotting.Properties
import qualified Test.Cardano.Chain.Slotting.Json
import qualified Test.Cardano.Chain.Ssc.CBOR
import qualified Test.Cardano.Chain.UTxO.CBOR
import qualified Test.Cardano.Chain.UTxO.Compact
import qualified Test.Cardano.Chain.UTxO.Json
import qualified Test.Cardano.Chain.UTxO.Model
import qualified Test.Cardano.Chain.Update.CBOR
import qualified Test.Cardano.Chain.Update.Json
import qualified Test.Cardano.Chain.Update.Properties
import qualified Test.Cardano.Chain.Elaboration.Delegation

main :: IO ()
main =
  mainWithTestScenario
    $   testGroup "Cardano Ledger Tests"
    $   tsGroupToTree
    <$> [ Test.Cardano.Chain.Block.CBOR.tests
        , Test.Cardano.Chain.Block.Validation.tests
        , Test.Cardano.Chain.Common.Address.tests
        , Test.Cardano.Chain.Common.CBOR.tests
        , Test.Cardano.Chain.Common.Compact.tests
        , Test.Cardano.Chain.Common.Json.tests
        , Test.Cardano.Chain.Common.Lovelace.tests
        , Test.Cardano.Chain.Delegation.CBOR.tests
        , const Test.Cardano.Chain.Delegation.Model.tests
        , const Test.Cardano.Chain.Epoch.File.tests
        , Test.Cardano.Chain.Elaboration.Delegation.tests
        , Test.Cardano.Chain.Genesis.Json.tests
        , Test.Cardano.Chain.Slotting.CBOR.tests
        , Test.Cardano.Chain.Slotting.Properties.tests
        , Test.Cardano.Chain.Slotting.Json.tests
        , const Test.Cardano.Chain.Ssc.CBOR.tests
        , Test.Cardano.Chain.UTxO.CBOR.tests
        , Test.Cardano.Chain.UTxO.Compact.tests
        , Test.Cardano.Chain.UTxO.Json.tests
        , Test.Cardano.Chain.UTxO.Model.tests
        , Test.Cardano.Chain.Update.CBOR.tests
        , Test.Cardano.Chain.Update.Json.tests
        , Test.Cardano.Chain.Update.Properties.tests
        ]

tsGroupToTree :: TSGroup -> TestTree
tsGroupToTree tsGroup = askOption $ \scenario -> case tsGroup scenario of
  Group { groupName, groupProperties } -> testGroup
    (unGroupName groupName)
    (uncurry testProperty . first unPropertyName <$> groupProperties)
