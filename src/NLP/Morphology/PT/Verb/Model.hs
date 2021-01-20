-- Copyright 2019 jjunho
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- You may obtain a copy of the License at
--
--     http://www.apache.org/licenses/LICENSE-2.0
--
-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS,
-- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
-- See the License for the specific language governing permissions and
-- limitations under the License.

{-# LANGUAGE OverloadedStrings #-}

module NLP.Morphology.PT.Verb.Model where

import           Data.Text                   (Text)
import           NLP.Morphology.PT.Verb.Base

data VForm
  = VImpr {
      vr :: Root
    , vt :: ThematicVowel
    , vm :: MoodTense
  }
  | VPers {
      vr :: Root
    , vt :: ThematicVowel
    , vm :: MoodTense
    , vp :: PersonNumber
  }
  | VPart {
      vr :: Root
    , vt :: ThematicVowel
    , vm :: MoodTense
    , vg :: Gender
    , vn :: Number
  }
  | VComp {
      v1 :: VForm
    , v2 :: VForm
  }
  | VNull
  deriving (Show, Eq)

data MForm
  = MImpr {
      mr, mt, mm :: Text
  }
  | MPers {
      mr, mt, mm, mp :: Text
  }
  | MPart {
      mr, mt, mm, mg, mn :: Text
  }
  | MComp {
      m1, m2 :: MForm
  }
  | MNull
  deriving (Show, Eq)

mkVImpr :: Citation -> MoodTense -> VForm
mkVImpr c = VImpr (mkRoot c) (mkThematicVowel c)

mkVPers :: Citation -> MoodTense -> PersonNumber -> VForm
mkVPers c = VPers (mkRoot c) (mkThematicVowel c)

mkVPart :: Citation -> MoodTense -> Gender -> Number -> VForm
mkVPart c = VPart (mkRoot c) (mkThematicVowel c)

mkVComp :: VForm -> VForm
mkVComp (VPers r t IFUT p) = VComp (VImpr r t INF) (VPers (Root Irr "HAVER" "H") E IPRS p)
mkVComp (VPers r t IFPR p) = VComp (VImpr r t INF) (VPers (Root Irr "HAVER" "H") E IIPF p)
mkVComp _ = error "Not possible to create a VForm"

mkMForm :: VForm -> MForm
mkMForm v = case v of
  VImpr r t m     -> MImpr (root r) (tv t) (mta m)
  VPers r t m p   -> MPers (root r) (tv t) (mta m) (pns p)
  VPart r t m g n -> MPart (root r) (tv t) (mta m) (gen g) (num n)
  VComp v1 v2     -> MComp (mkMForm v1) (mkMForm v2)
  VNull           -> MNull

toMorphs :: MForm -> [Text]
toMorphs mf = case mf of
  MImpr r t m     -> [r, t, m]
  MPers r t m p   -> [r, t, m, p]
  MPart r t m g n -> [r, t, m, g, n]
  MComp m1 m2     -> toMorphs m1 <> toMorphs m2
  MNull           -> [nullForm]
