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
       [endLine "This is a test line."]),
    ("breakExact empty still provides line", checkLineBreak
       breakExact
       ""
       [endLine ""]),
    ("breakExact exact multiple", checkLineBreak
       (setLineWidth breakExact 5)
       "This is a test line."
       [innerLine "This ",
        innerLine "is a ",
        innerLine "test ",
        endLine "line."]),
    ("breakExact not a multiple", checkLineBreak
       (setLineWidth breakExact 7)
       "This is a test line."
       [innerLine "This is",
        innerLine " a test",
        endLine " line."]),
    ("breakExact restores line", do
       let breaker = setLineWidth breakExact 7
       let line = "This is a test line."
       let restored = concat $ map vlText $ breakLines breaker line
       checkCondition (restored == line) line),
    ("hideLeadingSpace skips breaks by default", checkLineBreak
       hideLeadingSpace
       "This is a test line."
       [endLine "This is a test line."]),
    ("hideLeadingSpace empty still provides line", checkLineBreak
       hideLeadingSpace
       ""
       [endLine ""]),
    ("hideLeadingSpace exact multiple", checkLineBreak
       (setLineWidth hideLeadingSpace 5)
       "This is a test line."
       [innerLine "This ",
        innerLine "is a ",
        innerLine "test ",
        endLine "line."]),
    ("hideLeadingSpace not a multiple", checkLineBreak
       (setLineWidth hideLeadingSpace 7)
       "This is a test line."
       [innerLine "This is ",
        innerLine "a test ",
        endLine "line."]),
    ("hideLeadingSpace allows extra hidden spaces at end", checkLineBreak
       (setLineWidth hideLeadingSpace 5)
       "Here      are some extra spaces."
       [innerLine "Here      ",
        innerLine "are s",
        innerLine "ome e",
        innerLine "xtra ",
        innerLine "space",
        endLine "s."]),
    ("hideLeadingSpace restores line", do
       let breaker = setLineWidth hideLeadingSpace 7
       let line = "This is a test line."
       let restored = concat $ map vlText $ breakLines breaker line
       checkCondition (restored == line) line),
    ("hideLeadingSpace trims only trailing spaces", checkLineRender
       hideLeadingSpace
       (innerLine "  This line had extra spaces.  ")
       "  This line had extra spaces."),
    ("hideLeadingSpace trims skips paragraph end", checkLineRender
       hideLeadingSpace
       (endLine "  This line had extra spaces.  ")
       "  This line had extra spaces.  "),
    ("hideLeadingSpace no tweak in leading spaces", checkCursorTweak
       hideLeadingSpace
       (innerLine "  This line had extra spaces.  ")
       1 1),
    ("hideLeadingSpace no tweak in middle", checkCursorTweak
       hideLeadingSpace
       (innerLine "  This line had extra spaces.  ")
       10 10),
    ("hideLeadingSpace tweak at back", checkCursorTweak
       hideLeadingSpace
       (innerLine "  This line had extra spaces.  ")
       29 29),
    ("hideLeadingSpace tweak in trailing spaces", checkCursorTweak
       hideLeadingSpace
       (innerLine "  This line had extra spaces.  ")
       31 29),
    ("hideLeadingSpace tweak skipped at paragraph end", checkCursorTweak
       hideLeadingSpace
       (endLine "  This line had extra spaces.  ")
       31 31)
  ]

checkLineBreak b x ys = do
  let breaks = breakLines b x
  checkCondition (breaks == ys) (show breaks)

checkLineRender b x y = do
  let rendered = renderLine b x
  checkCondition (rendered == y) (show y)

checkCursorTweak b x k j = do
  let tweaked = tweakCursor b x k
  checkCondition (tweaked == j) (show tweaked)
