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

-- | Generic line-parsing functionality.

{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE TypeFamilies #-}

module WEditor.Base.Parser (
  FixedFontParser(..),
  emptyLine,
) where

import WEditor.Base.Line


-- | Line parser for fixed-width fonts.
class FixedFontParser a c | a -> c where
  -- | Type used to differentiate between line-break types.
  type BreakType a :: *
  -- | An acceptable fallback line-break type.
  defaultBreak :: a -> BreakType a
  -- | Change the max line width used for parsing. A width of zero must result
  --   in breakLines skipping line breaks.
  setLineWidth :: a -> Int -> a
  -- | Break the sequence into lines.
  --
  --   The following must hold for all possible inputs to a 'FixedFontParser'
  --   `p`:
  --
  --   prop> concat (map vlText (breakLines p line)) == line
  --
  --   Implement 'renderLine' and 'tweakCursor' to make visual adjustments (such
  --   as adding hyphens or indentation) if necessary.
  breakLines :: a -> [c] -> [VisibleLine c (BreakType a)]
  -- | Render the line for viewing. Implement 'tweakCursor' if 'renderLine'
  --   changes the positions of any characters on the line.
  renderLine :: a -> VisibleLine c (BreakType a) -> [c]
  -- | Adjust the horizontal cursor position.
  tweakCursor :: a -> VisibleLine c (BreakType a) -> Int -> Int
  tweakCursor _ _ = id

-- | Create an empty line.
emptyLine :: FixedFontParser a c => a -> VisibleLine c (BreakType a)
emptyLine p = VisibleLine (defaultBreak p) []
