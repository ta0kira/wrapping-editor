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
{-# LANGUAGE Safe #-}

module Base.Parser (
  FixedFontParser(..),
) where

import Base.Line


-- | Line parser for fixed-width fonts.
class DefaultBreak b => FixedFontParser a c b | a -> c b where
  -- | Change the max line width used for parsing. A width of zero must result
  --   in breakLines skipping line breaks.
  setLineWidth :: a -> Int -> a
  -- | Break the sequence into lines.
  breakLines :: a -> [c] -> [VisibleLine c b]
  -- | Render the line for viewing. Implement tweakCursor if renderLine changes
  --   the positions of any characters on the line.
  renderLine :: a -> VisibleLine c b -> [c]
  -- | Adjust the horizontal cursor position.
  tweakCursor :: a -> VisibleLine c b -> Int -> Int
  tweakCursor _ _ = id
