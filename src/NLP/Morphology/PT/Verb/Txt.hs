-- Copyright 2024 jjunho
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

module NLP.Morphology.PT.Verb.Txt (
  Txt(..)
  ) where

import           Data.Text                    (Text)
import qualified Data.Text                    as T
import           NLP.Morphology.PT.Verb.Base
import           NLP.Morphology.PT.Verb.Model

class (Show a) => Txt a where
  txt :: a -> Text
  txt = tshow

instance Txt ThematicVowel
instance Txt MoodTense
instance Txt PersonNumber
instance Txt Gender
instance Txt Number

instance Txt Root where
  txt = ("\8730" <>) . txt . root

instance Txt Text where
  txt = id

instance Txt VForm where
  txt (VImpr r t m    ) = T.intercalate "-" [txt r, txt t, txt m]
  txt (VPers r t m p  ) = T.intercalate "-" [txt r, txt t, txt m, txt p]
  txt (VPart r t m g n) = T.intercalate "-" [txt r, txt t, txt m, txt g, txt n]
  txt (VComp v1 v2    ) = T.intercalate "+" [txt v1, txt v2]
  txt  VNull            = nullForm

instance Txt MForm where
  txt (MImpr r t m    ) = T.intercalate "-" [txt r, txt t, txt m]
  txt (MPers r t m p  ) = T.intercalate "-" [txt r, txt t, txt m, txt p]
  txt (MPart r t m g n) = T.intercalate "-" [txt r, txt t, txt m, txt g, txt n]
  txt (MComp v1 v2    ) = T.intercalate "+" [txt v1, txt v2]
  txt  MNull            = nullForm

instance Txt a => Txt [a] where
  txt = T.intercalate "-" . fmap txt
