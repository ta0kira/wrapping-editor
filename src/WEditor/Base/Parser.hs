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
) where

import WEditor.Base.Line


-- | Line parser for fixed-width fonts.
--
--     * @p@: Parser type providing the operations.
--     * @c@: Character type.
class FixedFontParser p c | p -> c where
  -- | Type used to differentiate between line-break types.
  type BreakType p :: *
  -- | Change the max line width used for parsing. A width of zero must result
  --   in breakLines skipping line breaks.
  setLineWidth :: p -> Int -> p
  -- | Break the sequence into lines.
  --
  --   The following must hold for all possible inputs to a 'FixedFontParser'
  --   @p@:
  --
  --   prop> concat (map vlText (breakLines p l)) == l
  --
  --   Implement 'renderLine' and 'tweakCursor' to make visual adjustments (such
  --   as adding hyphens or indentation) if necessary.
  breakLines :: p -> [c] -> [VisibleLine c (BreakType p)]
  -- | A place-holder line for empty paragraphs.
  emptyLine :: p -> VisibleLine c (BreakType p)
  -- | Render the line for viewing. Implement 'tweakCursor' if 'renderLine'
  --   changes the positions of any characters on the line.
  renderLine :: p -> VisibleLine c (BreakType p) -> [c]
  -- | Adjust the horizontal cursor position.
  tweakCursor :: p -> VisibleLine c (BreakType p) -> Int -> Int
  tweakCursor _ _ = id
  -- | Split the line to create a paragraph break.
  --
  --   The following must hold for all possible inputs to a 'FixedFontParser'
  --   @p@:
  --
  --   prop> let (b,t) = splitLine p n l in vlText l == vlText b ++ vlText t
  splitLine :: p
            -> Int                           -- ^ Index to split at.
            -> VisibleLine c (BreakType p)   -- ^ Line to split.
            -> (VisibleLine c (BreakType p),
                VisibleLine c (BreakType p)) -- ^ New lines at @(bottom,top)@ of previous/next paragraphs.
