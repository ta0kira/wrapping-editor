{- -----------------------------------------------------------------------------
Copyright 2020 Kevin P. Barry

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
----------------------------------------------------------------------------- -}

-- Author: Kevin P. Barry [ta0kira@gmail.com]

-- | Language-specific hyphenation rules.

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Safe #-}

module WEditorHyphen.LangHyphen (
  LangHyphen,
  langHyphen,
) where

import Data.Char
import Data.List
import Text.Hyphenation
import WEditor.LineWrap


data LangHyphen = LangHyphen Language Hyphenator

-- | Hyphenates words using 'Language'-specific rules.
--
--   Example usage:
--
-- @
-- import Text.Hyphenation
-- import WEditor.Document
-- import WEditor.LineWrap
-- import WEditorHyphen.LangHyphen
--
-- content = map UnparsedPara (lines "Your document content.")
--
-- doc = editDocument (breakWords (langHyphen English_US)) content
-- @
langHyphen :: Language -> LangHyphen
langHyphen l = LangHyphen l (languageHyphenator l)


-- Private below here.

instance Show LangHyphen where
  show (LangHyphen l _) = show l

instance WordSplitter LangHyphen Char where
  splitWord (LangHyphen l h) k w cs
    | w < (minWidth l) || k > w = Nothing
    | k >= length cs || k < 3   = Just []
    | otherwise = Just breaks where
        (nb,cs',ne) = trimPunct l cs
        (n0:ns) = map length $ hyphenate h cs'
        breaks
          -- Move the word to the next line if it has punctuation in the middle.
          | any (noSplitChars l) cs' = []
          | null ns = []
          | otherwise = combine k (nb+n0) (init ns ++ [ne+last ns])
        combine _ _ [] = []
        combine t n (k:ks)
          -- Add a break if adding a segment would exceed the remaining space.
          | (n+k > t-(length (hyphenChar l)) && not (null ks)) || n+k > t = n:(combine w k ks)
          -- Append the next segment to the current segment.
          | otherwise = combine w (n+k) ks
  isWordChar (LangHyphen l _) = wordChars l
  isWhitespace (LangHyphen l _) = whitespaceChars l
  appendHyphen (LangHyphen l _) = (++ hyphenChar l)
  endsWithHyphen (LangHyphen l _) cs
    | null cs || null (hyphenChar l) = False
    | otherwise = hyphenChar l `isSuffixOf` cs

-- Set the language-specific minimum line width here.
minWidth :: Language -> Int
minWidth _ = 8

wordChars :: Language -> Char -> Bool
wordChars = check where
  -- Override the language-specific predicate here.
  check l@English_US c = checkDefault l c || c `elem` "'"
  check l@English_GB c = checkDefault l c || c `elem` "'"
  check l c = checkDefault l c
  -- Override the language-specific character categories here.
  cats _ = defaultCats
  -- Leave the stuff below here alone.
  checkDefault l c = generalCategory c `elem` cats l || noSplitChars l c
  defaultCats = [
      DashPunctuation,
      LowercaseLetter,
      ModifierLetter,
      NonSpacingMark,
      OtherLetter,
      SpacingCombiningMark,
      TitlecaseLetter,
      UppercaseLetter
    ]

noSplitChars :: Language -> Char -> Bool
noSplitChars = check where
  -- Override the language-specific predicate here.
  check l@English_US c = checkDefault l c && not (c `elem` "'")
  check l@English_GB c = checkDefault l c && not (c `elem` "'")
  check l c = checkDefault l c
  -- Override the language-specific character categories here.
  cats _ = defaultCats
  -- Leave the stuff below here alone.
  checkDefault l c = generalCategory c `elem` cats l
  defaultCats = [
      ConnectorPunctuation,
      CurrencySymbol,
      DecimalNumber,
      FinalQuote,
      InitialQuote,
      OtherNumber,
      OtherPunctuation
    ]

-- Set language-specific whitespace detection here.
whitespaceChars :: Language -> Char -> Bool
whitespaceChars _ c = isSeparator c

-- Set the language-specific hyphen char here.
hyphenChar :: Language -> [Char]
hyphenChar _ = "-"

trimPunct :: Language -> [Char] -> (Int,[Char],Int)
trimPunct l cs =
  (length $ takeWhile (noSplitChars l) cs,
   dropWhile (noSplitChars l) $ reverse $ dropWhile (noSplitChars l) $ reverse cs,
   length $ takeWhile (noSplitChars l) $ reverse cs)
