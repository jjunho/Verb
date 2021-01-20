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

{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE OverloadedStrings          #-}

module NLP.Morphology.PT.Verb (
    Citation
  , MoodTense(..)
  , PersonNumber(..)
  , Tense(..)
  , Paradigm(..)
  , VForm
  -- , VForm
  , mkParadigm
  , mkTense
  , deep
  , suppletiveForms
  , shallow
  , shallowOrth
  , orthMorphs
  , orth
  -- , txt
  , Txt(..)
  , bounds
  , (<$$>)
  , takeFrom
) where

import qualified Data.Text                    as T

import           NLP.Morphology.PT.Verb.Base
import           NLP.Morphology.PT.Verb.DSO
import           NLP.Morphology.PT.Verb.Model
import           NLP.Morphology.PT.Verb.Txt

newtype Tense a
  = Tense { unTense :: [a] }
  deriving (Functor, Show, Eq)

newtype Paradigm a
  = Paradigm { unParadigm :: [a] }
  deriving (Functor, Show, Eq)

mkTense :: Citation -> MoodTense -> Tense VForm
mkTense c m
  | m `elem` [INF, GER] = Tense [mkVImpr c m]
  | m == PPP            = Tense [mkVPart c m g n | n <- bounds, g <- bounds]
  | otherwise           = Tense [mkVPers c m p | p <- bounds]

mkParadigm :: Citation -> Paradigm (Tense VForm)
mkParadigm c = Paradigm [mkTense c m | m <- bounds ]

instance Txt a => Txt (Tense a) where
  txt = T.intercalate "\n" . fmap txt . unTense

instance Txt a => Txt (Paradigm a) where
  txt = T.intercalate "\n\n" . fmap txt . unParadigm
