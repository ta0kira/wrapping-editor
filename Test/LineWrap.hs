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
       [newLine "This is a test line."]),
    ("breakExact empty still provides line", checkLineBreak
       breakExact
       ""
       [newLine ""]),
    ("breakExact exact multiple", checkLineBreak
       (setLineWidth breakExact 5)
       "This is a test line."
       [newLine "This ",
        newLine "is a ",
        newLine "test ",
        newLine "line."]),
    ("breakExact not a multiple", checkLineBreak
       (setLineWidth breakExact 7)
       "This is a test line."
       [newLine "This is",
        newLine " a test",
        newLine " line."]),
    ("breakExact restores line", do
       let breaker = setLineWidth breakExact 7
       let line = "This is a test line."
       let restored = concat $ map vlText $ breakLines breaker line
       checkCondition (restored == line) line),
    ("trimSpaces skips breaks by default", checkLineBreak
       trimSpaces
       "This is a test line."
       [newLine "This is a test line."]),
    ("trimSpaces empty still provides line", checkLineBreak
       trimSpaces
       ""
       [newLine ""]),
    ("trimSpaces exact multiple", checkLineBreak
       (setLineWidth trimSpaces 5)
       "This is a test line."
       [newLine "This ",
        newLine "is a ",
        newLine "test ",
        newLine "line."]),
    ("trimSpaces not a multiple", checkLineBreak
       (setLineWidth trimSpaces 7)
       "This is a test line."
       [newLine "This is ",
        newLine "a test ",
        newLine "line."]),
    ("trimSpaces restores line", do
       let breaker = setLineWidth trimSpaces 7
       let line = "This is a test line."
       let restored = concat $ map vlText $ breakLines breaker line
       checkCondition (restored == line) line),
    ("trimSpaces trims only trailing spaces", checkLineRender
       trimSpaces
       (newLine "  This line had extra spaces.  ")
       "  This line had extra spaces."),
    ("trimSpaces no tweak in leading spaces", checkCursorTweak
       trimSpaces
       (newLine "  This line had extra spaces.  ")
       1 1),
    ("trimSpaces no tweak in middle", checkCursorTweak
       trimSpaces
       (newLine "  This line had extra spaces.  ")
       10 10),
    ("trimSpaces tweak at back", checkCursorTweak
       trimSpaces
       (newLine "  This line had extra spaces.  ")
       29 29),
    ("trimSpaces tweak in trailing spaces", checkCursorTweak
       trimSpaces
       (newLine "  This line had extra spaces.  ")
       31 29)
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
