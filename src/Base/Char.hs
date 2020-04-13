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

-- | Features of character sets.

{-# LANGUAGE Safe #-}

module Base.Char (
  HyphenChar(..),
  SpaceChar(..),
  WordChar(..),
  countLeadingSpaces,
  trimLeadingSpaces,
) where

import Data.Char


-- | Dealing with space characters.
class SpaceChar c where
  -- | Predicate for identifying space characters.
  isSpaceChar :: c -> Bool

-- | Dealing with word characters.
class SpaceChar c => WordChar c where
  -- | Predicate for identifying word characters.
  isWordChar :: c -> Bool

-- | Supporting hyphenation.
class HyphenChar c where
  -- | The canonical hyphen character.
  hyphenChar :: c

-- | Trims spaces from the front of a sequence.
trimLeadingSpaces :: SpaceChar c => [c] -> [c]
trimLeadingSpaces = dropWhile isSpaceChar

-- | Counts spaces at the front of a sequence.
countLeadingSpaces :: SpaceChar c => [c] -> Int
countLeadingSpaces = length . takeWhile isSpaceChar

instance SpaceChar Char where
  isSpaceChar = (== ' ')

instance WordChar Char where
  isWordChar = isAlpha

instance HyphenChar Char where
  hyphenChar = '-'
