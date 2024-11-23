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

module Main where

import qualified Data.Text              as T
import           Lucid
import qualified NLP.Morphology.PT.Verb as V
import qualified Web.Scotty             as Scotty

main :: IO ()
main =
  Scotty.scotty 8123 $ do
    Scotty.get "/" $ Scotty.html $ renderText noVerbPage
    Scotty.get "/p/:citation" $ do
      c <- Scotty.captureParam "citation"
      Scotty.html . renderText $ pageComplete c
    Scotty.get "/s/:citation" $ do
      c <- Scotty.captureParam "citation"
      Scotty.html . renderText $ pageShallowOrth c
    Scotty.get "/:citation" $ do
      c <- Scotty.captureParam "citation"
      Scotty.html . renderText $ pageOrth c

hero, h1Title, box, h1p, h2p, h3p, h1k, h2k, h3k, section :: Html () -> Html ()
hero =
  section_ [class_ "hero is-dark is-bold"] .
  div_ [class_ "hero-body"] . div_ [class_ "container"] . h1Title

h1Title = h1_ [class_ "title is-1"]

box = div_ [class_ "box"]

h1p = p_ [class_ "title is-2"]

h2p = p_ [class_ "title is-3"]

h3p = p_ [class_ "title is-4"]

h1k = p_ [class_ "subtitle is-4"]

h2k = p_ [class_ "subtitle is-5"]

h3k = p_ [class_ "subtitle is-6"]

section = section_ [class_ "section"] . div_ [class_ "container"]

pgHead :: Html ()
pgHead =
  head_ $ do
    meta_ [charset_ "utf-8"]
    meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1"]
    link_
      [ rel_ "stylesheet"
      , href_
          "https://cdnjs.cloudflare.com/ajax/libs/bulma/0.7.5/css/bulma.min.css"
      ]
    title_ "Conjuga\231\227o de verbos em português"

noVerbPage :: Html ()
noVerbPage =
  doctypehtml_ $ do
    pgHead
    body_ $ do
      hero "Conjuga\231\227o de verbos em português"
      section $ do
        h1p "Exemplos"
        box $ do
          p_ [class_ "title is-4"] "falar"
          ul_ $ do
            li_ $ a_ [href_ "/falar"] "Conjugação"
            li_ $ a_ [href_ "/s/falar"] "Análise simples"
            li_ $ a_ [href_ "/p/falar"] "Análise profunda"
        box $ do
          p_ [class_ "title is-4"] "comer"
          ul_ $ do
            li_ $ a_ [href_ "/comer"] "Conjugação"
            li_ $ a_ [href_ "/s/comer"] "Análise simples"
            li_ $ a_ [href_ "/p/comer"] "Análise profunda"
        box $ do
          p_ [class_ "title is-4"] "partir"
          ul_ $ do
            li_ $ a_ [href_ "/partir"] "Conjugação"
            li_ $ a_ [href_ "/s/partir"] "Análise simples"
            li_ $ a_ [href_ "/p/partir"] "Análise profunda"
      footer

pageComplete :: V.Citation -> Html ()
pageComplete c = do
  let tvt = tableVdsoTense c
  page c tvt

pageShallowOrth :: V.Citation -> Html ()
pageShallowOrth c = do
  let tvt = tableSOTense c
  page c tvt

pageOrth :: V.Citation -> Html ()
pageOrth c = do
  let tvt = tableOTense c
  page c tvt

formBox :: Html () -> Html () -> Html ()
formBox p k = do
  h1p p
  h1k k

moodBox :: Html () -> Html () -> Html () -> Html ()
moodBox p k t =
  box $ do
    h2p p
    h2k k
    t

tenseBox :: Html () -> Html () -> Html () -> Html ()
tenseBox p k t =
  box $ do
    h3p p
    h3k k
    t

footer :: Html ()
footer =
  div_ [class_ "footer"] $
  p_ [class_ "has-text-centered"] "Copyright (C) 2019 Prof. Juliano"

page :: T.Text -> (V.MoodTense -> Html ()) -> Html ()
page c tvt =
  doctypehtml_ $ do
    pgHead
    body_ $ do
      hero (toHtml ("Conjuga\231\227o do verbo \"" <> T.toTitle c <> "\""))
      section $ do
        formBox "Formas Finitas" "정형(定形)"
        moodBox "Modo Indicativo" "직설법(直說法)" (indicative tvt)
        moodBox "Modo Subjuntivo" "접속법(接續法)" (subjunctive tvt)
        moodBox "Modo Imperativo" "명령법(命令法)" (imperative tvt)
        formBox "Formas Infinitas" "부정형(不定形)"
        moodBox "Infinitivo" "부정사(不定詞) [명사(名詞)]" (tvt V.INF)
        moodBox "Infinitivo Pessoal" "인칭부정사(人稱不定詞) [명사(名詞)]" (tvt V.INFP)
        moodBox "Gerúndio" "능동태 현재분사(能動態 現在分詞) [부사(副詞)/형용사(形容詞)]" (tvt V.GER)
        moodBox
          "Particípio Passivo Passado"
          "수동태 과거분사(受動態 過去分詞) [형용사(形容詞)/부사(副詞)]"
          (tvt V.PPP)
    footer

indicative :: (V.MoodTense -> Html ()) -> Html ()
indicative tvt = do
  tenseBox "Presente" "현재(現在)" (tvt V.IPRS)
  tenseBox "Pretérito Perfeito" "완료과거(完了過去)/완전과거(完全過去)" (tvt V.IPRF)
  tenseBox "Pretérito Imperfeito" "미완료과거(未完了過去)/불완전과거(不完全過去)" (tvt V.IIPF)
  tenseBox "Pretérito Mais que Perfeito" "대과거(大過去)" (tvt V.IPPF)
  tenseBox "Futuro do Presente" "현재미래(現在未來)" (tvt V.IFUT)
  tenseBox "Futuro do Pretérito" "과거미래(過去未來)/조건법 현재(條件法 現在)" (tvt V.IFPR)

subjunctive :: (V.MoodTense -> Html ()) -> Html ()
subjunctive tvt = do
  tenseBox "Presente" "현재(現在)" (tvt V.SPRS)
  tenseBox "Pretérito Imperfeito" "미완료과거(未完了過去)/불완전과거(不完全過去)" (tvt V.SIPF)
  tenseBox "Futuro" "미래(未來)" (tvt V.SFUT)

imperative :: (V.MoodTense -> Html ()) -> Html ()
imperative tvt = do
  tenseBox "Afirmativo" "긍정(肯定)" (tvt V.IMPA)
  tenseBox "Negativo" "부정(否定)" (tvt V.IMPN)

tense :: V.Citation -> V.MoodTense -> V.Tense V.VForm
tense = V.takeFrom . V.unParadigm . V.mkParadigm

vdso :: V.VForm -> [T.Text]
vdso v =
  [ a
  , if b == a
      then ""
      else b
  , if c == b
      then ""
      else c
  , if d == c
      then ""
      else d
  , if e == d
      then ""
      else e
  , if f == e
      then ""
      else f
  , if g == f
      then ""
      else g
  ]
  where
    a = V.txt v
    b = (V.txt . V.suppletiveForms) v
    c = (V.txt . V.deep) v
    d = (V.txt . V.shallow) v
    e = (V.txt . V.shallowOrth) v
    f = (V.txt . V.orthMorphs) v
    g = (V.txt . V.orth) v

so :: V.VForm -> [T.Text]
so v =
  [ a
  , if b == a
      then ""
      else b
  , if c == b
      then ""
      else c
  , if d == c
      then ""
      else d
  ]
  where
    a = V.txt v
    b = (V.txt . V.shallowOrth) v
    c = (V.txt . V.orthMorphs) v
    d = (V.txt . V.orth) v

o :: V.VForm -> [T.Text]
o v = [(V.txt . V.orth) v]

vdsos :: V.Tense V.VForm -> [[T.Text]]
vdsos = V.unTense . fmap vdso

sos :: V.Tense V.VForm -> [[T.Text]]
sos = V.unTense . fmap so

os :: V.Tense V.VForm -> [[T.Text]]
os = V.unTense . fmap o

table :: [[T.Text]] -> Html ()
table t =
  table_ [class_ "table is-hoverable is-fullwidth is-striped"] $ mapM_ row t

row :: [T.Text] -> Html ()
row xs = tr_ [] $ mapM_ (td_ [] . toHtml) xs

tableVdsoTense :: V.Citation -> V.MoodTense -> Html ()
tableVdsoTense c = table . vdsos . tense c

tableSOTense :: V.Citation -> V.MoodTense -> Html ()
tableSOTense c = table . sos . tense c

tableOTense :: V.Citation -> V.MoodTense -> Html ()
tableOTense c = table . os . tense c
