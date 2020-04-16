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

-- | Simple representation of viewable text lines.

{-# LANGUAGE Safe #-}

module WEditor.Base.Line (
  VisibleLine(..),
) where


-- | Line meant for viewing.
data VisibleLine c b =
  VisibleLine {
    vlBreak :: b, -- ^ The type of line break for this line.
    vlText :: [c] -- ^ The complete data of the line.
  }
  deriving (Eq,Ord,Show)

-- | Break type that has a default.
class DefaultBreak b where
  -- | The default break for the break type.
  defaultBreak :: b
