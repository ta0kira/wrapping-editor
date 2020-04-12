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
  LineBreak(..),
  breakExact,
  hideLeadingSpace,
) where

import Base.Line
import Base.Para
import Base.Parser


data LineBreak = ParagraphEnd | SimpleBreak deriving (Eq,Ord,Show)

instance DefaultBreak LineBreak where
  defaultBreak = ParagraphEnd

newtype BreakExact c = BreakExact Int deriving (Show)

breakExact :: BreakExact c
breakExact = BreakExact 0

newtype TrimSpaces c = TrimSpaces Int deriving (Show)

hideLeadingSpace :: TrimSpaces c
hideLeadingSpace = TrimSpaces 0

instance FixedFontParser (BreakExact c) c LineBreak where
  setLineWidth _ w = BreakExact w
  breakLines (BreakExact w) = breakCommon (const False) w
  renderLine _ = vlText

instance SpaceChar c => FixedFontParser (TrimSpaces c) c LineBreak where
  setLineWidth _ w = TrimSpaces w
  breakLines (TrimSpaces w) = breakCommon isSpaceChar w
  renderLine (TrimSpaces w) (VisibleLine cs ParagraphEnd)
    | w < 1 = cs
    | otherwise = take w cs
  renderLine _ (VisibleLine cs SimpleBreak)  = reverse $ trimSpacesFront $ reverse cs
  tweakCursor (TrimSpaces w) (VisibleLine _ ParagraphEnd)
    | w < 1 = id
    | otherwise = min w
  tweakCursor _ (VisibleLine cs _) = max 0 . min (total-post) where
    post = countSpacesFront $ reverse cs
    total = length cs

class SpaceChar c where
  isSpaceChar :: c -> Bool

trimSpacesFront :: SpaceChar c => [c] -> [c]
trimSpacesFront = dropWhile isSpaceChar

countSpacesFront :: SpaceChar c => [c] -> Int
countSpacesFront = length . takeWhile isSpaceChar

breakCommon :: (c -> Bool) -> Int -> [c] -> [VisibleLine c LineBreak]
breakCommon f w [] = [emptyLine]
breakCommon f w cs
  | w < 1 = [VisibleLine cs ParagraphEnd]
  | otherwise = breakOrEmpty cs where
    breakOrEmpty [] = []
    breakOrEmpty cs = adjust (reverse $ take w cs) (drop w cs) where
      adjust line rest
        | null rest =
          [VisibleLine (reverse line) ParagraphEnd]
        | not (f $ head rest) =
          (VisibleLine (reverse line) SimpleBreak):(breakOrEmpty rest)
        | otherwise = adjust (head rest:line) (tail rest)

instance SpaceChar Char where
  isSpaceChar = (== ' ')
