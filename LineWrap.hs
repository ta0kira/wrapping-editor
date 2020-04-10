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
) where

import Base.Line
import Base.Para
import Base.Parser


data LineBreak = SimpleBreak | HiddenSpace | WordHyphen deriving (Enum,Eq,Ord,Show)

data BreakExact c = BreakExact Int deriving (Show)

breakExact :: BreakExact c
breakExact = BreakExact 0

instance FixedFontParser (BreakExact c) c LineBreak where
  setLineWidth _ w = BreakExact w
  breakLines _ [] = [emptyLine]
  breakLines (BreakExact w) cs
    | w < 1 = [VisibleLine cs SimpleBreak]
    | otherwise = breakOrEmpty cs where
      breakOrEmpty [] = []
      breakOrEmpty cs = line:(breakOrEmpty rest) where
        line = VisibleLine (take w cs) SimpleBreak
        rest = drop w cs
  joinLines _ = concat . map vlText
  renderLine _ = vlText
