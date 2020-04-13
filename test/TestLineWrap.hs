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

module TestLineWrap (
  allTests,
) where

import LineWrap
import Base.Line
import Base.Parser

import Common


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
    ("breakExact preserves data", do
       let breaker = setLineWidth breakExact 7
       let line = "This is a test line."
       let restored = concat $ map vlText $ breakLines breaker line
       checkCondition (show restored) (restored == line)),
    ("noHyphen skips breaks by default", checkLineBreak
       (breakWords noHyphen)
       "This is a test line."
       [endLine "This is a test line."]),
    ("noHyphen empty still provides line", checkLineBreak
       (breakWords noHyphen)
       ""
       [endLine ""]),
    ("noHyphen exact multiple", checkLineBreak
       (setLineWidth (breakWords noHyphen) 5)
       "This is a test line."
       [innerLine "This ",
        innerLine "is a ",
        innerLine "test ",
        endLine "line."]),
    ("noHyphen not a multiple", checkLineBreak
       (setLineWidth (breakWords noHyphen) 7)
       "This is a test line."
       [innerLine "This is ",
        innerLine "a test ",
        endLine "line."]),
    ("noHyphen allows extra hidden spaces at end", checkLineBreak
       (setLineWidth (breakWords noHyphen) 5)
       "Here      are some extra spaces alongwordthatwillnotfit.      "
       [innerLine "Here      ",
        innerLine "are ",
        innerLine "some ",
        innerLine "extra ",
        innerLine "space",
        innerLine "s ",
        innerLine "along",
        innerLine "wordt",
        innerLine "hatwi",
        innerLine "llnot",
        endLine "fit.      "]),
    ("noHyphen preserves data", do
       let breaker = setLineWidth (breakWords noHyphen) 5
       let line = "Here      are some extra spaces.      "
       let restored = concat $ map vlText $ breakLines breaker line
       checkCondition (show restored) (restored == line)),
    ("lazyHyphen skips short words", checkWordBreaks
       (splitWord lazyHyphen 0 7) "the" $ Just []),
    ("lazyHyphen with single break", checkWordBreaks
       (splitWord lazyHyphen 4 7) "hyphenate" $ Just [3]),
    ("lazyHyphen with multiple break", checkWordBreaks
       (splitWord lazyHyphen 4 5) "hyphenation" $ Just [3,4]),
    ("lazyHyphen skips short break", checkWordBreaks
       (splitWord lazyHyphen 2 5) "hyphenate" $ Just []),
    ("lazyHyphen skips avoids short end", checkWordBreaks
       (splitWord lazyHyphen 9 10) "hyphenate" $ Just []),
    ("lazyHyphen skips breaks by default", checkLineBreak
       (breakWords lazyHyphen)
       "This is a test line."
       [endLine "This is a test line."]),
    ("lazyHyphen empty still provides line", checkLineBreak
       (breakWords lazyHyphen)
       ""
       [endLine ""]),
    ("lazyHyphen exact multiple", checkLineBreak
       (setLineWidth (breakWords lazyHyphen) 5)
       "This is a test line."
       [innerLine "This ",
        innerLine "is a ",
        innerLine "test ",
        endLine "line."]),
    ("lazyHyphen not a multiple", checkLineBreak
       (setLineWidth (breakWords lazyHyphen) 7)
       "This is a test line."
       [innerLine "This is ",
        innerLine "a test ",
        endLine "line."]),
    ("lazyHyphen allows extra hidden spaces after last break", checkLineBreak
       (setLineWidth (breakWords lazyHyphen) 7)
       "Here      are some extra spaces and alongwordthatwillnotfit.      "
        [innerLine "Here      ",
         hyphenLine "are so",
         hyphenLine "me ext",
         hyphenLine "ra spa",
         innerLine "ces and ",
         hyphenLine "alongw",
         hyphenLine "ordtha",
         hyphenLine "twilln",
         endLine "otfit.      "]),
    ("lazyHyphen breaks long word in middle of line", checkLineBreak
       (setLineWidth (breakWords lazyHyphen) 7)
       "   averylongword"
       [hyphenLine "   ave",
        hyphenLine "rylong",
        endLine "word"]),
    ("lazyHyphen breaks word bumped to next line", checkLineBreak
       (setLineWidth (breakWords lazyHyphen) 5)
       "   averylongword"
       [innerLine "   ",
        hyphenLine "aver",
        hyphenLine "ylon",
        endLine "gword"]),
    ("lazyHyphen preserves data", do
       let breaker = setLineWidth (breakWords lazyHyphen) 7
       let line = "Here      are some extra spaces and alongwordthatwillnotfit.      "
       let restored = concat $ map vlText $ breakLines breaker line
       checkCondition (show restored) (restored == line)),
    ("breakWords trims only trailing spaces", checkLineRender
       (breakWords noHyphen)
       (innerLine "  This line had extra spaces.  ")
       "  This line had extra spaces."),
    ("breakWords trims allows spaces at paragraph end", checkLineRender
       (setLineWidth (breakWords noHyphen) 30)
       (endLine "  This line had extra spaces.  ")
       "  This line had extra spaces. "),
    ("breakWords adds a hyphen", checkLineRender
       (breakWords lazyHyphen)
       (hyphenLine "somet")
       "somet-"),
    ("breakWords adds a hyphen even with spaces", checkLineRender
       (breakWords lazyHyphen)
       (hyphenLine "weird ")
       "weird -"),
    ("breakWords no tweak in leading spaces", checkCursorTweak
       (breakWords noHyphen)
       (innerLine "  This line had extra spaces.  ")
       1 1),
    ("breakWords no tweak in middle", checkCursorTweak
       (breakWords noHyphen)
       (innerLine "  This line had extra spaces.  ")
       10 10),
    ("breakWords tweak at back", checkCursorTweak
       (breakWords noHyphen)
       (innerLine "  This line had extra spaces.  ")
       29 29),
    ("breakWords tweak in trailing spaces", checkCursorTweak
       (breakWords noHyphen)
       (innerLine "  This line had extra spaces.  ")
       31 29),
    ("breakWords tweak allows spaces at paragraph end", checkCursorTweak
       (setLineWidth (breakWords noHyphen) 30)
       (endLine "  This line had extra spaces.  ")
       31 30)
  ]

checkLineBreak b x ys = do
  let breaks = breakLines b x
  checkCondition (show breaks) (breaks == ys)

checkLineRender b x y = do
  let rendered = renderLine b x
  checkCondition (show rendered) (rendered == y)

checkCursorTweak b x k j = do
  let tweaked = tweakCursor b x k
  checkCondition (show tweaked) (tweaked == j)

checkWordBreaks f x y = do
  let breaks = f x
  checkCondition (show breaks) (breaks == y)
