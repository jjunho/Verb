{-# LANGUAGE OverloadedStrings #-}

module RootSpec where

import           Data.Text              (Text)
import qualified Data.Text              as T
import           NLP.Morphology.PT.Verb (Root (..), RootType (..), getRoot,
                                         mkRoot)
import           Test.Hspec

spec :: Spec
spec = do
  describe "getRoot" $ do
    it "extrai a raiz de uma forma de citação" $ do
      getRoot "CANTAR" `shouldBe` "CANT"
      getRoot "PÔR" `shouldBe` "P"
    it "converte para maiúsculas e extrai a raiz" $ do
      getRoot "cantar" `shouldBe` "CANT"
      getRoot "pôr" `shouldBe` "P"
  describe "mkRoot" $ do
    it "converte para maiúsculas e extrai a raiz" $ do
      mkRoot "cantar" `shouldBe` Root Reg "CANT" "CANT"
    it "detecta irregularidades" $ do
      mkRoot "pescar" `shouldBe` Root CQU "PESC" "PESQU"
