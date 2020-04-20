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

module WEditor.Base.Char (
  HyphenChar(..),
  WhitespaceChar(..),
  WordChar(..),
) where

import Data.Char


-- | Dealing with whitespace characters.
class WhitespaceChar c where
  -- | Predicate for identifying whitespace characters.
  defaultIsWhitespace :: c -> Bool

-- | Dealing with word characters.
class WhitespaceChar c => WordChar c where
  -- | Predicate for identifying word characters.
  defaultIsWordChar :: c -> Bool

-- | Supporting hyphenation.
class HyphenChar c where
  -- | The canonical hyphen character.
  defaultHyphen :: c
  isDefaultHyphen :: c -> Bool
  isDefaultHyphen _ = False

instance WhitespaceChar Char where
  defaultIsWhitespace = (== ' ')

instance WordChar Char where
  defaultIsWordChar = flip any [isAlpha,(`elem` ".'-")] . flip ($)

instance HyphenChar Char where
  defaultHyphen = '-'
  isDefaultHyphen = (== defaultHyphen)
