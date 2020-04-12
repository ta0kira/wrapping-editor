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

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Safe #-}

module LineWrap (
  BreakExact,
  BreakWords,
  LineBreak,
  breakExact,
  breakWords,
  lazyHyphen,
  lineBreakEnd,
  lineBreakHyphen,
  lineBreakSimple,
  noHyphen,
) where

import Control.Applicative ((<|>))

import Base.Char
import Base.Line
import Base.Para
import Base.Parser


data LineBreak = ParagraphEnd | SimpleBreak | HyphenatedWord deriving (Eq,Ord,Show)

lineBreakEnd :: LineBreak
lineBreakEnd = ParagraphEnd

lineBreakSimple :: LineBreak
lineBreakSimple = SimpleBreak

lineBreakHyphen :: LineBreak
lineBreakHyphen = HyphenatedWord

newtype BreakExact c = BreakExact Int deriving (Show)

breakExact :: BreakExact c
breakExact = BreakExact 0

type WordSplitter c = Int -> Int -> [c] -> [[c]]

data BreakWords c = BreakWords Int (WordSplitter c)

breakWords :: (WordChar c, HyphenChar c) => WordSplitter c -> BreakWords c
breakWords = BreakWords 0

noHyphen :: WordSplitter c
noHyphen _ _ _ = []

lazyHyphen :: WordSplitter c
lazyHyphen k w cs
  | w < 4 || k > w = []
  | k >= length cs || k < 3 = [cs]
  | otherwise = (take (k-1) cs):(splitBy w $ drop (k-1) cs) where
    splitBy _ [] = []
    splitBy n cs
      | length cs > n = (take (n-1) cs):(splitBy n $ drop (n-1) cs)
      | otherwise = [cs]


-- Private below here.

instance DefaultBreak LineBreak where
  defaultBreak = lineBreakEnd

instance FixedFontParser (BreakExact c) c LineBreak where
  setLineWidth _ w = BreakExact w
  breakLines _ [] = [emptyLine]
  breakLines (BreakExact w) cs
    | w < 1 = [VisibleLine cs lineBreakEnd]
    | otherwise = breakOrEmpty cs where
      breakOrEmpty [] = []
      breakOrEmpty cs = continue (reverse $ take w cs) (drop w cs) where
        continue ls [] = [VisibleLine (reverse ls) lineBreakEnd]
        continue ls rs = (VisibleLine (reverse ls) lineBreakSimple):(breakOrEmpty rs)
  renderLine _ = vlText

instance (WordChar c, HyphenChar c) => FixedFontParser (BreakWords c) c LineBreak where
  setLineWidth (BreakWords _ f) w = BreakWords w f
  breakLines (BreakWords w f) = breakAllLines w f
  renderLine (BreakWords w _) (VisibleLine cs ParagraphEnd)
    | w < 1 = cs
    | otherwise = take w cs
  renderLine _ (VisibleLine cs SimpleBreak) = reverse $ trimLeadingSpaces $ reverse cs
  renderLine _ (VisibleLine cs HyphenatedWord) = cs ++ [hyphenChar]
  tweakCursor (BreakWords w _) (VisibleLine _ ParagraphEnd)
    | w < 1 = id
    | otherwise = min w
  tweakCursor _ (VisibleLine cs SimpleBreak) = max 0 . min (total-post) where
    post = countLeadingSpaces $ reverse cs
    total = length cs
  tweakCursor _ (VisibleLine cs HyphenatedWord) = id

breakAllLines :: WordChar c => Int -> WordSplitter c -> [c] -> [VisibleLine c LineBreak]
breakAllLines _ _ [] = [emptyLine]
breakAllLines w f cs
  | w < 1 = [VisibleLine cs lineBreakEnd]
  | otherwise = breakOrEmpty cs where
      breakOrEmpty cs = let (Just ls) = handleSplit (reverse $ take w cs) (drop w cs) in ls
      handleSplit line rest =
        tryWord line rest <|>
        trySpaces line rest <|>
        lineDefault line rest
      lineDefault []  _ = Just []
      lineDefault ls [] = Just [VisibleLine (reverse ls) lineBreakEnd]
      lineDefault ls rs = Just $ VisibleLine (reverse ls) lineBreakSimple:(breakOrEmpty rs)
      tryWord ls@(l:_) rs@(r:_) | isWordChar l && isWordChar r = newLines where
        ls2 = dropWhile isWordChar ls
        rs2 = dropWhile isWordChar rs
        wordFront = reverse $ takeWhile isWordChar ls
        wordBack = takeWhile isWordChar rs
        breaks = f (length wordFront) w (wordFront ++ wordBack)
        newLines
          -- Splitter refuses to deal with the word.
          | null breaks = Nothing
          -- If there is no split, move the whole word to the next line.
          | length breaks == 1 = handleSplit ls2 (head breaks ++ rs2)
          | otherwise = Just $ hyphenate ((reverse ls2 ++ head breaks):(tail breaks)) rs2
        hyphenate bs rs =
          map (flip VisibleLine lineBreakHyphen) (init bs) ++
          -- Last break goes with the rest of the next line.
          breakOrEmpty (last bs ++ rs)
      tryWord _ _ = Nothing
      trySpaces ls rs@(r:_) | isSpaceChar r = newLines where
        ls' = reverse ls ++ takeWhile isSpaceChar rs
        rs' = dropWhile isSpaceChar rs
        newLines
          | null rs'  = Just [VisibleLine ls' lineBreakEnd]
          | otherwise = Just $ (VisibleLine ls' lineBreakSimple):(breakOrEmpty rs')
      trySpaces _ _ = Nothing
