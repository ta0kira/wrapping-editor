{- -----------------------------------------------------------------------------
Copyright 2020 Kevin P. Barry

Licensed under the Apache License, Version 2.0 (the "License");
you may not use This file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
----------------------------------------------------------------------------- -}

-- Author: Kevin P. Barry [ta0kira@gmail.com]

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Safe #-}

module TestLangHyphen (
  allTests,
) where

import Text.Hyphenation
import WEditor.Base
import WEditor.LineWrap
import WEditorHyphen.LangHyphen

import Common


allTests :: [(String,IO (Maybe String))]
allTests = tests_English_US

tests_English_US :: [(String,IO (Maybe String))]
tests_English_US = [
    ("langHyphen English_US without punctuation", checkLineBreak
       (setLineWidth (breakWords (langHyphen English_US)) 8)
       "Hyphenation testability depends on the usage of longer lexicographical objects"
       [hyphenLine "Hyphen",
        innerLine "ation ",
        hyphenLine "testa",
        innerLine "bility ",
        innerLine "depends ",
        innerLine "on the ",
        innerLine "usage of ",
        innerLine "longer ",
        hyphenLine "lexico",
        hyphenLine "graphi",
        hyphenLine "cal ob",
        endLine "jects"]),
    ("langHyphen English_US with quoted text", checkLineBreak
       (setLineWidth (breakWords (langHyphen English_US)) 8)
       "Hyphenation \"testability\" depends on the usage of longer \"lexicographical objects\""
       [hyphenLine "Hyphen",
        innerLine"ation ",
        hyphenLine "\"testa",
        innerLine "bility\" ",
        innerLine "depends ",
        innerLine "on the ",
        innerLine "usage of ",
        innerLine "longer ",
        hyphenLine "\"lexico",
        hyphenLine "graphi",
        hyphenLine "cal ob",
        endLine "jects\""]),
    ("langHyphen English_US leading quote not left hanging", checkLineBreak
       (setLineWidth (breakWords (langHyphen English_US)) 8)
       "012345 \"somethingness\""
       [innerLine "012345 ",
        hyphenLine "\"some",
        hyphenLine "thing",
        endLine "ness\""]),
    ("langHyphen English_US trailing quote and period not left hanging", checkLineBreak
       (setLineWidth (breakWords (langHyphen English_US)) 8)
       "    \"The.\""
       [innerLine "    ",
        endLine "\"The.\""])
  ]

checkLineBreak b x ys = do
  let breaks = breakLines b x
  checkCondition (show breaks) (breaks == ys)
