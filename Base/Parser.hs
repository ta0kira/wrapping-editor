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

{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Safe #-}

module Base.Parser (
  FixedFontParser(..),
) where

import Base.Line


class DefaultBreak b => FixedFontParser a c b | a -> c b where
  setLineWidth :: a -> Int -> a
  breakLines :: a -> [c] -> [VisibleLine c b]
  renderLine :: a -> VisibleLine c b -> [c]
  tweakCursor :: a -> VisibleLine c b -> Int -> Int
  tweakCursor _ _ = id
