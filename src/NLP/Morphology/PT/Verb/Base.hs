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

module NLP.Morphology.PT.Verb.Base where

import           Data.List (elemIndex)
import           Data.Text (Text)
import qualified Data.Text as T

tshow :: Show a => a -> Text
tshow = T.pack . show

tread :: Read a => Text -> a
tread = read . T.unpack

bounds :: (Enum a, Bounded a) => [a]
bounds = [minBound .. maxBound]

(<$$>) :: (Functor f1, Functor f2) => (a -> b) -> f1 (f2 a) -> f1 (f2 b)
(<$$>) = fmap . fmap

zero :: Text
zero = "Ø"

nullForm :: Text
nullForm = "---"

type Citation = Text

data Root
  = Root { rootType :: RootType
         , root     :: Text
         , orthRoot :: Text
         }
  deriving (Show, Eq)

data ThematicVowel
  = A
  | E
  | I
  | O
  | U
  | Z
  deriving (Show, Eq, Enum, Bounded)

data MoodTense
  = IPRS
  | IPRF
  | IIPF
  | IPPF
  | IFUT
  | IFPR
  | SPRS
  | SIPF
  | SFUT
  | IMPA
  | IMPN
  | INFP
  | INF
  | GER
  | PPP
  deriving (Show, Eq, Enum, Bounded)

data PersonNumber
  = P1
  | P2
  | P3
  | P4
  | P5
  | P6
  deriving (Show, Eq, Enum, Bounded)

data Gender
  = MSC
  | FEM
  deriving (Show, Eq, Enum, Bounded)

data Number
  = SG
  | PL
  deriving (Show, Eq, Enum, Bounded)

data GenderNumber
  = MS
  | FS
  | MP
  | FP
  deriving (Show, Eq, Enum, Bounded)

data RootType
  = Reg
  | Irr
  | CQU
  | QUC
  | GGU
  | GUG
  | CÇ
  | ÇC
  | GJ
  deriving (Show, Eq)

getRoot :: Citation -> Text
getRoot = T.dropEnd 2 . T.toUpper

mkRoot :: Citation -> Root
mkRoot c
  | suffix "CAR"  = Root CQU r (chgR CQU r)
  | suffix "QUER" = Root QUC r (chgR GUG r)
  | suffix "QUIR" = Root QUC r (chgR GUG r)
  | suffix "GAR"  = Root GGU r (chgR GGU r)
  | suffix "GUER" = Root GUG r (chgR GUG r)
  | suffix "GUIR" = Root GUG r (chgR GUG r)
  | suffix "ÇAR"  = Root ÇC  r (chgR ÇC  r)
  | suffix "CER"  = Root CÇ  r (chgR CÇ  r)
  | suffix "CIR"  = Root CÇ  r (chgR CÇ  r)
  | suffix "GER"  = Root GJ  r (chgR GJ  r)
  | suffix "GIR"  = Root GJ  r (chgR GJ  r)
  | otherwise     = Root Reg r r
  where
    c'       = T.toUpper c
    r        = getRoot c'
    suffix l = l `T.isSuffixOf` c'

chgR :: RootType -> Text -> Text
chgR rt r = case rt of
  Reg -> r
  Irr -> r
  CQU -> (<> "QU") $ d 1 r
  QUC -> (<> "C")  $ d 2 r
  GGU -> (<> "GU") $ d 1 r
  GUG -> (<> "G")  $ d 2 r
  ÇC  -> (<> "C")  $ d 1 r
  CÇ  -> (<> "Ç")  $ d 1 r
  GJ  -> (<> "J")  $ d 1 r
 where d = T.dropEnd

mkThematicVowel :: Citation -> ThematicVowel
mkThematicVowel c = if ocirc $ tv c then O else tv' c
  where tv = T.head . T.takeEnd 2 . T.toLower
        tv' = maybe Z toEnum . flip elemIndex "aeiou" . tv
        ocirc = ('ô' ==)

takeFrom :: Enum a => [b] -> a -> b
takeFrom ms = (ms !!) . fromEnum

toZero :: Text -> Text
toZero "0" = zero
toZero x   = x

gw :: Enum a => Text -> a -> Text
gw = takeFrom . fmap toZero . T.words

tv :: ThematicVowel -> Text
tv = gw "A E I O U 0"

pns, prs, des, imp, prf :: PersonNumber -> Text
pns = gw "0 S   0 MOS IS   M"
prs = gw "O S   0 MOS IS   M"
des = gw "0 S   0 MOS DES  M"
imp = gw "X 0   X X   I    X"
prf = gw "I STE U MOS STES M"

mta, mte, mta2, mte2 :: MoodTense -> Text
mta  = gw "0 0 VA RA 0 0 0 SE R 0 0 R R NDO D"
mte  = gw ": : VE RE : : : :  : : : : : :   :"
mta2 = gw ": : A  :  : : : :  : : : : : :   T"
mte2 = gw ": : E  :  : : : :  : : : : : :   :"

gen :: Gender -> Text
gen = gw "O A"

num :: Number -> Text
num = gw "0 S"
