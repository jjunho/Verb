{-# LANGUAGE OverloadedStrings #-}

module VFormSpec
  ( spec
  ) where

import           Data.Text              (Text)
import           NLP.Morphology.PT.Verb (MoodTense (..), PersonNumber (..),
                                         Root (..), RootType (..),
                                         ThematicVowel (..), VForm (..),
                                         mkVImpr, mkVPers)
import           Test.Hspec

spec :: Spec
spec = do
  describe "mkVImpr" $ do
    it "cria corretamente uma forma imperativa" $ do
      let result = mkVImpr "CANTAR" INF
      result `shouldBe` VImpr (Root Reg "CANT" "CANT") A INF
  describe "mkVPers" $ do
    it "cria corretamente uma forma pessoal" $ do
      let result = mkVPers "COMER" IPRS P1
      result `shouldBe` VPers (Root Reg "COM" "COM") E IPRS P1
