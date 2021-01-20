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

module NLP.Morphology.PT.Verb.DSO where

import           Data.Text                    (Text)
import qualified Data.Text                    as T
import           NLP.Morphology.PT.Verb.Base
import           NLP.Morphology.PT.Verb.Model

deep :: VForm -> MForm
deep = deepM . suppletiveForms

deepM :: VForm -> MForm
deepM v = case v of
  VPers r t IPRF p  -> MPers (root r) (tv t) (mta IPRF) (prf p)
  VPers r A IIPF p  -> MPers (root r) (tv A) (mta IIPF) (pns p)
  VPers r t IIPF p  -> MPers (root r) (tv I) (mta2 IIPF) (pns p)
  VPers r t IMPA p  -> MPers (root r) (tv t) (mta IMPA) (imp p)
  VPart r E PPP g n -> MPart (root r) (tv I) (mta PPP) (gen g) (num n)
  VComp v1 v2       -> MComp (deepM v1) (deepM v2)
  _                 -> mkMForm v

suppletiveForms :: VForm -> VForm
suppletiveForms v@(VPers r t m p) = case (t, m, p) of
  (_, IMPA, P1) -> VNull
  (_, IMPN, P1) -> VNull
  (_, IFUT, _ ) -> mkVComp v
  (_, IFPR, _ ) -> mkVComp v
  (_, IPRF, P6) -> vp t IPPF P6
  (A, SPRS, _ ) -> vp E m p
  (_, SPRS, _ ) -> vp A m p
  (_, IMPA, P3) -> suppletiveForms $ vp t SPRS p
  (_, IMPA, P4) -> suppletiveForms $ vp t SPRS p
  (_, IMPA, P6) -> suppletiveForms $ vp t SPRS p
  (_, IMPN, _ ) -> suppletiveForms $ vp t SPRS p
  _             -> v
  where vp = VPers r
suppletiveForms v = v

paradigmaticIrregularity :: VForm -> MForm
paradigmaticIrregularity v@(VPers r t m p) = case (t, m, p) of
  (A, IPRS, P1) -> mp Z (prs p)
  (_, IPRS, P1) -> mo Z (prs p)
  (I, IPRS, P2) -> mp E (prs p)
  (I, IPRS, P3) -> mp E (prs p)
  (I, IPRS, P5) -> mp Z (prs p)
  (I, IPRS, P6) -> mp E (prs p)
  (A, IPRF, P1) -> mo E (prf p)
  (A, IPRF, P3) -> mp O (prf p)
  (_, IPRF, P1) -> mp Z (prf p)
  (_, IPRF, _ ) -> mp t (prf p)
  (A, IIPF, P5) -> me  t (pns p)
  (_, IIPF, P5) -> me2 t (pns p)
  (_, IPPF, P5) -> me  t (pns p)
  (A, IIPF, _ ) -> mf v
  (_, IIPF, _ ) -> mi t (pns p)
  (_, SPRS, _ ) -> mo t (pns p)
  (_, SFUT, _ ) -> mp t (des p)
  (I, IMPA, P2) -> mp E (imp p)
  (I, IMPA, P5) -> mp Z (imp p)
  (_, IMPA, _ ) -> mp t (imp p)
  (_, INFP, _ ) -> mp t (des p)
  _             -> mf v
  where mp t = MPers (root r) (tv t) (mta m)
        mo t = MPers (orthRoot r) (tv t) (mta m)
        mi _ = MPers (root r) (tv I) (mta2 m)
        me t = MPers (root r) (tv t) (mte m)
        me2 t = MPers (root r) (tv I) (mte2 m)
        mf = mkMForm
paradigmaticIrregularity v = case v of
  VPart r E m g n -> MPart (root r) (tv I) (mta m) (gen g) (num n)
  VComp v1 v2     -> MComp (deep v1) (dfut v2)
  VNull           -> MNull
  _               -> deep v

dfut :: VForm -> MForm
dfut v@(VPers r t m p) = case (m, p) of
  (IPRS, P1) -> mp1 E
  (IPRS, P4) -> mp2 E
  (IPRS, P5) -> mp2 E
  (IPRS, _ ) -> mp2 A
  (IIPF, _ ) -> mp3 I
  where mp1 t = MPers (orthRoot r) (tv t) (mta  m) (prf p)
        mp2 t = MPers (orthRoot r) (tv t) (mta  m) (pns p)
        mp3 t = MPers (orthRoot r) (tv t) (mta2 m) (pns p)

shallow :: VForm -> MForm
shallow = paradigmaticIrregularity . suppletiveForms

shfut :: VForm -> MForm
shfut v = case v of
  VPers r t m@IIPF p@P5 -> MPers zero (tv I) (mte2 m) (pns p)
  VPers r t m      p    -> dfut (VPers (Root Irr zero zero) t m p)

shallowOrth :: VForm -> MForm
shallowOrth v = case v of
  VPers r t IIPF P4 -> vpAcute v'
  VPers r t IIPF P5 -> vpAcute v'
  VPers r E IPPF P4 -> vpCirc  v'
  VPers r E IPPF P5 -> vpCirc  v'
  VPers r t IPPF P4 -> vpAcute v'
  VPers r t IPPF P5 -> vpAcute v'
  VPers r E SIPF P4 -> sse $ vpCirc  v'
  VPers r E SIPF P5 -> sse $ vpCirc  v'
  VPers r t SIPF P4 -> sse $ vpAcute v'
  VPers r t SIPF P5 -> sse $ vpAcute v'
  VPers r t SIPF p  -> sse v'
  VPers r t SFUT P2 -> e v'
  VPers r t SFUT P6 -> e v'
  VPers r t INFP P2 -> e v'
  VPers r t INFP P6 -> e v'
  VPers r t IFUT p -> shallowOrth $ mkVComp v
  VPers r t IFPR p -> shallowOrth $ mkVComp v
  VComp v1 v2       -> MComp (shallowOrth v1) (sofut v2)
  _                 -> v'
  where
    v' = shallow v
    sse (MPers r t m p) = MPers r (t <> "S") m p
    e   (MPers r t m p) = MPers r t m ("E" <> p)

sofut :: VForm -> MForm
sofut v = case v of
  VPers r t m@IPRS p@P2 -> MPers zero "Á" (mta m) (pns p)
  VPers r t m@IPRS p@P3 -> MPers zero "Á" (mta m) (pns p)
  VPers r t m@IPRS p@P6 -> MPers zero "Ã" (mta m) "O"
  VPers r t m@IIPF p@P4 -> vpAcute $ shfut v
  VPers r t m@IIPF p@P5 -> vpAcute $ shfut v
  VPers r t m      p    -> shfut v

acute :: Text -> Text
acute tv = case tv of
  "A" -> "Á"
  "E" -> "É"
  "I" -> "Í"
  "O" -> "Ó"
  "U" -> "Ú"
  _   -> error $ "Acute in non-vowel: " ++ T.unpack tv

circ :: Text -> Text
circ tv = case tv of
  "A" -> "Â"
  "E" -> "Ê"
  "O" -> "Ô"

vpAcute :: MForm -> MForm
vpAcute (MPers r t m p) = MPers r (acute t) m p

vpCirc :: MForm -> MForm
vpCirc (MPers r t m p) = MPers r (circ  t) m p

orthMorphs :: VForm -> [Text]
orthMorphs = filter (/= zero) . toMorphs . shallowOrth

orth :: VForm -> Text
orth = T.toLower . T.intercalate "" . orthMorphs
