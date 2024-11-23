{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE OverloadedStrings #-}

module NLP.Morphology.PT.Verb where

import           Data.List  (elemIndex)
import           Data.Maybe (fromMaybe)
import           Data.Text  (Text)
import qualified Data.Text  as T

-- Funções genéricas auxiliares
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

-- Definições de tipos de dados
type Citation = Text

data Root =
  Root
    { rootType :: RootType
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

-- Funções de manipulação de raízes
getRoot :: Citation -> Text
getRoot = T.dropEnd 2 . T.toUpper

mkRoot :: Citation -> Root
mkRoot c
  | suffix "CAR" = Root CQU r (chgR CQU r)
  | suffix "QUER" = Root QUC r (chgR GUG r)
  | suffix "QUIR" = Root QUC r (chgR GUG r)
  | suffix "GAR" = Root GGU r (chgR GGU r)
  | suffix "GUER" = Root GUG r (chgR GUG r)
  | suffix "GUIR" = Root GUG r (chgR GUG r)
  | suffix "ÇAR" = Root ÇC r (chgR ÇC r)
  | suffix "CER" = Root CÇ r (chgR CÇ r)
  | suffix "CIR" = Root CÇ r (chgR CÇ r)
  | suffix "GER" = Root GJ r (chgR GJ r)
  | suffix "GIR" = Root GJ r (chgR GJ r)
  | otherwise = Root Reg r r
  where
    c' = T.toUpper c
    r = getRoot c'
    suffix l = l `T.isSuffixOf` c'

chgR :: RootType -> Text -> Text
chgR rt r =
  case rt of
    Reg -> r
    Irr -> r
    CQU -> (<> "QU") $ d 1 r
    QUC -> (<> "C") $ d 2 r
    GGU -> (<> "GU") $ d 1 r
    GUG -> (<> "G") $ d 2 r
    ÇC  -> (<> "C") $ d 1 r
    CÇ  -> (<> "Ç") $ d 1 r
    GJ  -> (<> "J") $ d 1 r
  where
    d = T.dropEnd

-- Funções de manipulação de vogais temáticas
mkThematicVowel :: Citation -> ThematicVowel
mkThematicVowel c =
  if ocirc $ tv c
    then O
    else tv' c
  where
    tv = T.head . T.takeEnd 2 . T.toLower
    tv' = maybe Z toEnum . flip elemIndex "aeiou" . tv
    ocirc = ('ô' ==)

-- Funções de manipulação de morfemas
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
mta = gw "0 0 VA RA 0 0 0 SE R 0 0 R R NDO D"

mte = gw ": : VE RE : : : :  : : : : : :   :"

mta2 = gw ": : A  :  : : : :  : : : : : :   T"

mte2 = gw ": : E  :  : : : :  : : : : : :   :"

gen :: Gender -> Text
gen = gw "O A"

num :: Number -> Text
num = gw "0 S"

-- Definições de formas verbais
data VForm
  = VImpr
      { vr :: Root
      , vt :: ThematicVowel
      , vm :: MoodTense
      }
  | VPers
      { vr :: Root
      , vt :: ThematicVowel
      , vm :: MoodTense
      , vp :: PersonNumber
      }
  | VPart
      { vr :: Root
      , vt :: ThematicVowel
      , vm :: MoodTense
      , vg :: Gender
      , vn :: Number
      }
  | VComp
      { v1, v2 :: VForm
      }
  | VNull
  deriving (Show, Eq)

data MForm
  = MImpr
      { mr, mt, mm :: Text
      }
  | MPers
      { mr, mt, mm, mp :: Text
      }
  | MPart
      { mr, mt, mm, mg, mn :: Text
      }
  | MComp
      { m1, m2 :: MForm
      }
  | MNull
  deriving (Show, Eq)

-- Funções de manipulação de formas verbais
mkVImpr :: Citation -> MoodTense -> VForm
mkVImpr c = VImpr (mkRoot c) (mkThematicVowel c)

mkVPers :: Citation -> MoodTense -> PersonNumber -> VForm
mkVPers c = VPers (mkRoot c) (mkThematicVowel c)

mkVPart :: Citation -> MoodTense -> Gender -> Number -> VForm
mkVPart c = VPart (mkRoot c) (mkThematicVowel c)

mkVComp :: VForm -> VForm
mkVComp (VPers r t IFUT p) =
  VComp (VImpr r t INF) (VPers (Root Irr "HAVER" "H") E IPRS p)
mkVComp (VPers r t IFPR p) =
  VComp (VImpr r t INF) (VPers (Root Irr "HAVER" "H") E IIPF p)
mkVComp _ = error "Not possible to create a VForm"

mkMForm :: VForm -> MForm
mkMForm v =
  case v of
    VImpr r t m     -> MImpr (root r) (tv t) (mta m)
    VPers r t m p   -> MPers (root r) (tv t) (mta m) (pns p)
    VPart r t m g n -> MPart (root r) (tv t) (mta m) (gen g) (num n)
    VComp v1 v2     -> MComp (mkMForm v1) (mkMForm v2)
    VNull           -> MNull

-- Conversão de estruturas verbais em morfemas
toMorphs :: MForm -> [Text]
toMorphs mf =
  case mf of
    MImpr r t m     -> [r, t, m]
    MPers r t m p   -> [r, t, m, p]
    MPart r t m g n -> [r, t, m, g, n]
    MComp m1 m2     -> toMorphs m1 <> toMorphs m2
    MNull           -> [nullForm]

deep :: VForm -> MForm
deep = deepM . suppletiveForms

deepM :: VForm -> MForm
deepM v =
  case v of
    VPers r t IPRF p  -> MPers (root r) (tv t) (mta IPRF) (prf p)
    VPers r A IIPF p  -> MPers (root r) (tv A) (mta IIPF) (pns p)
    VPers r t IIPF p  -> MPers (root r) (tv I) (mta2 IIPF) (pns p)
    VPers r t IMPA p  -> MPers (root r) (tv t) (mta IMPA) (imp p)
    VPart r E PPP g n -> MPart (root r) (tv I) (mta PPP) (gen g) (num n)
    VComp v1 v2       -> MComp (deepM v1) (deepM v2)
    _                 -> mkMForm v

suppletiveForms :: VForm -> VForm
suppletiveForms v@(VPers r t m p) =
  case (t, m, p) of
    (_, IMPA, P1) -> VNull
    (_, IMPN, P1) -> VNull
    (_, IFUT, _)  -> mkVComp v
    (_, IFPR, _)  -> mkVComp v
    (_, IPRF, P6) -> vp t IPPF P6
    (A, SPRS, _)  -> vp E m p
    (_, SPRS, _)  -> vp A m p
    (_, IMPA, P3) -> suppletiveForms $ vp t SPRS p
    (_, IMPA, P4) -> suppletiveForms $ vp t SPRS p
    (_, IMPA, P6) -> suppletiveForms $ vp t SPRS p
    (_, IMPN, _)  -> suppletiveForms $ vp t SPRS p
    _             -> v
  where
    vp = VPers r
suppletiveForms v = v

paradigmaticIrregularity :: VForm -> MForm
paradigmaticIrregularity v@(VPers r t m p) =
  case (t, m, p) of
    (A, IPRS, P1) -> mp Z (prs p)
    (_, IPRS, P1) -> mo Z (prs p)
    (I, IPRS, P2) -> mp E (prs p)
    (I, IPRS, P3) -> mp E (prs p)
    (I, IPRS, P5) -> mp Z (prs p)
    (I, IPRS, P6) -> mp E (prs p)
    (A, IPRF, P1) -> mo E (prf p)
    (A, IPRF, P3) -> mp O (prf p)
    (_, IPRF, P1) -> mp Z (prf p)
    (_, IPRF, _)  -> mp t (prf p)
    (A, IIPF, P5) -> me t (pns p)
    (_, IIPF, P5) -> me2 t (pns p)
    (_, IPPF, P5) -> me t (pns p)
    (A, IIPF, _)  -> mf v
    (_, IIPF, _)  -> mi t (pns p)
    (_, SPRS, _)  -> mo t (pns p)
    (_, SFUT, _)  -> mp t (des p)
    (I, IMPA, P2) -> mp E (imp p)
    (I, IMPA, P5) -> mp Z (imp p)
    (_, IMPA, _)  -> mp t (imp p)
    (_, INFP, _)  -> mp t (des p)
    _             -> mf v
  where
    mp t = MPers (root r) (tv t) (mta m)
    mo t = MPers (orthRoot r) (tv t) (mta m)
    mi _ = MPers (root r) (tv I) (mta2 m)
    me t = MPers (root r) (tv t) (mte m)
    me2 t = MPers (root r) (tv I) (mte2 m)
    mf = mkMForm
paradigmaticIrregularity v =
  case v of
    VPart r E m g n -> MPart (root r) (tv I) (mta m) (gen g) (num n)
    VComp v1 v2     -> MComp (deep v1) (dfut v2)
    VNull           -> MNull
    _               -> deep v

dfut :: VForm -> MForm
dfut v@(VPers r t m p) =
  case (m, p) of
    (IPRS, P1) -> mp1 E
    (IPRS, P4) -> mp2 E
    (IPRS, P5) -> mp2 E
    (IPRS, _)  -> mp2 A
    (IIPF, _)  -> mp3 I
  where
    mp1 t = MPers (orthRoot r) (tv t) (mta m) (prf p)
    mp2 t = MPers (orthRoot r) (tv t) (mta m) (pns p)
    mp3 t = MPers (orthRoot r) (tv t) (mta2 m) (pns p)

shallow :: VForm -> MForm
shallow = paradigmaticIrregularity . suppletiveForms

shfut :: VForm -> MForm
shfut v =
  case v of
    VPers r t m@IIPF p@P5 -> MPers zero (tv I) (mte2 m) (pns p)
    VPers r t m p         -> dfut (VPers (Root Irr zero zero) t m p)

shallowOrth :: VForm -> MForm
shallowOrth v =
  case v of
    VPers r t IIPF P4 -> vpAcute v'
    VPers r t IIPF P5 -> vpAcute v'
    VPers r E IPPF P4 -> vpCirc v'
    VPers r E IPPF P5 -> vpCirc v'
    VPers r t IPPF P4 -> vpAcute v'
    VPers r t IPPF P5 -> vpAcute v'
    VPers r E SIPF P4 -> sse $ vpCirc v'
    VPers r E SIPF P5 -> sse $ vpCirc v'
    VPers r t SIPF P4 -> sse $ vpAcute v'
    VPers r t SIPF P5 -> sse $ vpAcute v'
    VPers r t SIPF p  -> sse v'
    VPers r t SFUT P2 -> e v'
    VPers r t SFUT P6 -> e v'
    VPers r t INFP P2 -> e v'
    VPers r t INFP P6 -> e v'
    VPers r t IFUT p  -> shallowOrth $ mkVComp v
    VPers r t IFPR p  -> shallowOrth $ mkVComp v
    VComp v1 v2       -> MComp (shallowOrth v1) (sofut v2)
    _                 -> v'
  where
    v' = shallow v
    sse (MPers r t m p) = MPers r (t <> "S") m p
    e (MPers r t m p) = MPers r t m ("E" <> p)

sofut :: VForm -> MForm
sofut v =
  case v of
    VPers r t m@IPRS p@P2 -> MPers zero "Á" (mta m) (pns p)
    VPers r t m@IPRS p@P3 -> MPers zero "Á" (mta m) (pns p)
    VPers r t m@IPRS p@P6 -> MPers zero "Ã" (mta m) "O"
    VPers r t m@IIPF p@P4 -> vpAcute $ shfut v
    VPers r t m@IIPF p@P5 -> vpAcute $ shfut v
    VPers r t m p         -> shfut v

acute :: Text -> Text
acute v =
  fromMaybe (error $ "Acute in non-vowel: " ++ T.unpack v) $
  lookup v [("A", "Á"), ("E", "É"), ("I", "Í"), ("O", "Ó"), ("U", "Ú")]

circ :: Text -> Text
circ v =
  fromMaybe (error $ "Circumflex in non-vowel: " ++ T.unpack v) $
  lookup v [("A", "Â"), ("E", "Ê"), ("O", "Ô")]

vpAcute :: MForm -> MForm
vpAcute (MPers r t m p) = MPers r (acute t) m p

vpCirc :: MForm -> MForm
vpCirc (MPers r t m p) = MPers r (circ t) m p

orthMorphs :: VForm -> [Text]
orthMorphs = filter (/= zero) . toMorphs . shallowOrth

orth :: VForm -> Text
orth = T.toLower . T.intercalate "" . orthMorphs

-- Definições de tipos de dados auxiliares para construção de paradigmas
newtype Tense a =
  Tense
    { unTense :: [a]
    }
  deriving (Functor, Show, Eq)

newtype Paradigm a =
  Paradigm
    { unParadigm :: [a]
    }
  deriving (Functor, Show, Eq)

mkTense :: Citation -> MoodTense -> Tense VForm
mkTense c m
  | m `elem` [INF, GER] = Tense [mkVImpr c m]
  | m == PPP = Tense [mkVPart c m g n | n <- bounds, g <- bounds]
  | otherwise = Tense [mkVPers c m p | p <- bounds]

mkParadigm :: Citation -> Paradigm (Tense VForm)
mkParadigm c = Paradigm [mkTense c m | m <- bounds]

-- Definição de classe de tipos de dados para transformá-los em texto
class (Show a) =>
      Txt a
  where
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
  txt (VImpr r t m)     = T.intercalate "-" [txt r, txt t, txt m]
  txt (VPers r t m p)   = T.intercalate "-" [txt r, txt t, txt m, txt p]
  txt (VPart r t m g n) = T.intercalate "-" [txt r, txt t, txt m, txt g, txt n]
  txt (VComp v1 v2)     = T.intercalate "+" [txt v1, txt v2]
  txt VNull             = nullForm

instance Txt MForm where
  txt (MImpr r t m)     = T.intercalate "-" [txt r, txt t, txt m]
  txt (MPers r t m p)   = T.intercalate "-" [txt r, txt t, txt m, txt p]
  txt (MPart r t m g n) = T.intercalate "-" [txt r, txt t, txt m, txt g, txt n]
  txt (MComp v1 v2)     = T.intercalate "+" [txt v1, txt v2]
  txt MNull             = nullForm

instance Txt a => Txt [a] where
  txt = T.intercalate "-" . fmap txt

instance Txt a => Txt (Tense a) where
  txt = T.intercalate "\n" . fmap txt . unTense

instance Txt a => Txt (Paradigm a) where
  txt = T.intercalate "\n\n" . fmap txt . unParadigm
