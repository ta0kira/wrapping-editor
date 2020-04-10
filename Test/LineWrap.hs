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

{-# LANGUAGE Safe #-}

module Test.LineWrap (
  allTests,
) where

import LineWrap
import Base.Line
import Base.Parser
import Test.Common


allTests :: [(String,IO (Maybe String))]
allTests = [
    ("breakExact skips breaks by default", checkLineBreak
       breakExact
       "This is a test line."
       [VisibleLine "This is a test line." SimpleBreak]),
    ("breakExact empty still provides line", checkLineBreak
       breakExact
       ""
       [VisibleLine "" SimpleBreak]),
    ("breakExact exact multiple", checkLineBreak
       (setLineWidth breakExact 5)
       "This is a test line."
       [VisibleLine "This " SimpleBreak,
        VisibleLine "is a " SimpleBreak,
        VisibleLine "test " SimpleBreak,
        VisibleLine "line." SimpleBreak]),
    ("breakExact not a multiple", checkLineBreak
       (setLineWidth breakExact 7)
       "This is a test line."
       [VisibleLine "This is" SimpleBreak,
        VisibleLine " a test" SimpleBreak,
        VisibleLine " line." SimpleBreak]),
    ("breakExact restores line", do
       let breaker = setLineWidth breakExact 7
       let line = "This is a test line."
       let restored = joinLines breaker $ breakLines breaker line
       checkCondition (restored == line) line)
  ]

checkLineBreak b x ys = do
  let breaks = breakLines b x
  checkCondition (breaks == ys) (show breaks)
