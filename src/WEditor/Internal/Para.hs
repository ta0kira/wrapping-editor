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

-- | This module is for internal use.

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Safe #-}

module WEditor.Internal.Para (
  EditingPara,
  VisibleParaAfter,
  VisibleParaBefore,
  appendToPara,
  atParaBack,
  atParaFront,
  countLinesAfter,
  countLinesBefore,
  editPara,
  getAfterLines,
  getBeforeLines,
  getCurrentLine,
  getParaCharCount,
  getParaCursorChar,
  getParaCursorLine,
  getParaEditChar,
  modifyPara,
  moveParaCursor,
  paraCursorMovable,
  parseParaAfter,
  parseParaBefore,
  prependToPara,
  reparsePara,
  seekParaBack,
  seekParaFront,
  setParaCursorChar,
  setParaEditChar,
  splitPara,
  takeLinesAfter,
  takeLinesBefore,
  unparsePara,
  unparseParaAfter,
  unparseParaBefore,
  viewAfterLines,
  viewBeforeLines,
  viewParaAfter,
  viewParaBefore,
) where

import WEditor.Base.Editor
import WEditor.Base.Line
import WEditor.Base.Para
import WEditor.Base.Parser
import WEditor.Internal.Line


data VisibleParaBefore c b =
  VisibleParaBefore {
    vpbLines :: [VisibleLine c b],  -- Reversed.
    vpbSize :: Int
  }
  deriving (Show)

data VisibleParaAfter c b =
  VisibleParaAfter {
    vpaLines :: [VisibleLine c b]
  }
  deriving (Show)

data EditingPara c b =
  EditingPara {
    epBefore :: [VisibleLine c b],  -- Reversed.
    epEditing :: EditingLine c b,
    epAfter :: [VisibleLine c b],
    epSizeBefore :: Int
  }
  deriving (Show)

viewBeforeLines :: VisibleParaBefore c b -> [VisibleLine c b]
viewBeforeLines = reverse . vpbLines

viewAfterLines :: VisibleParaAfter c b -> [VisibleLine c b]
viewAfterLines = vpaLines

visibleParaBefore :: [VisibleLine c b] -> VisibleParaBefore c b
visibleParaBefore ls = VisibleParaBefore ls (sum $ map (length . vlText) ls)

parseParaBefore :: FixedFontParser a c => a -> UnparsedPara c -> VisibleParaBefore c (BreakType a)
parseParaBefore parser (UnparsedPara cs) =
  VisibleParaBefore (reverse $ breakLines parser cs) (length cs)

parseParaAfter :: FixedFontParser a c => a -> UnparsedPara c -> VisibleParaAfter c (BreakType a)
parseParaAfter parser (UnparsedPara cs) = VisibleParaAfter $ breakLines parser cs

unparseParaBefore :: VisibleParaBefore c b -> UnparsedPara c
unparseParaBefore (VisibleParaBefore ls _) = UnparsedPara $ joinLines $ reverse ls

unparseParaAfter :: VisibleParaAfter c b -> UnparsedPara c
unparseParaAfter (VisibleParaAfter ls) = UnparsedPara $ joinLines ls

editPara :: FixedFontParser a c => a -> UnparsedPara c -> EditingPara c (BreakType a)
editPara parser (UnparsedPara cs) = EditingPara [] (editLine line) after 0 where
  (line:after) = nonempty $ breakLines parser cs
  nonempty [] = [emptyLine parser]
  nonempty ls = ls

unparsePara :: EditingPara c b -> UnparsedPara c
unparsePara (EditingPara bs l as _) = UnparsedPara $ joinLines ls where
  ls = reverse bs ++ [viewLine l] ++ as

reparsePara :: FixedFontParser a c =>
  a -> EditingPara c (BreakType a) -> EditingPara c (BreakType a)
reparsePara parser (EditingPara bs l as n) = reparseParaTail parser revised where
  revised = EditingPara bs2 l2 as (sum $ map (length . vlText) bs2)
  bs' = reverse $ breakLines parser $ joinLines (reverse bs)
  (l2,bs2)
    | null bs' = (l,[])
    | otherwise = (head bs' `prependToLine` l,tail bs')

viewParaBefore :: EditingPara c b -> VisibleParaBefore c b
viewParaBefore (EditingPara bs l as _) = visibleParaBefore ls where
  ls = reverse as ++ [viewLine l] ++ bs

viewParaAfter :: EditingPara c b -> VisibleParaAfter c b
viewParaAfter (EditingPara bs l as _) = VisibleParaAfter ls where
  ls = reverse bs ++ [viewLine l] ++ as

getBeforeLines :: EditingPara c b -> VisibleParaBefore c b
getBeforeLines = visibleParaBefore . epBefore

getCurrentLine :: EditingPara c b -> VisibleLine c b
getCurrentLine = viewLine . epEditing

getAfterLines :: EditingPara c b -> VisibleParaAfter c b
getAfterLines = VisibleParaAfter . epAfter

takeLinesBefore :: Int -> [VisibleParaBefore c b] -> [VisibleLine c b]
takeLinesBefore n = reverse . take n . concat . map vpbLines

takeLinesAfter :: Int -> [VisibleParaAfter c b] -> [VisibleLine c b]
takeLinesAfter n = take n . concat . map vpaLines

-- TODO: Add an upper bound, to avoid unnecessary traversal.
countLinesBefore :: [VisibleParaBefore c b] -> Int
countLinesBefore = length . concat . map vpbLines

-- TODO: Add an upper bound, to avoid unnecessary traversal.
countLinesAfter :: [VisibleParaAfter c b] -> Int
countLinesAfter = length . concat . map vpaLines

getParaCursorLine :: EditingPara c b -> Int
getParaCursorLine = length . epBefore

getParaCursorChar :: EditingPara c b -> Int
getParaCursorChar = getLineCursor . epEditing

setParaCursorChar :: Int -> EditingPara c b -> EditingPara c b
setParaCursorChar k e@(EditingPara bs l as n) = (EditingPara bs (setLineCursor k l) as n)

getParaCharCount :: EditingPara c b -> Int
getParaCharCount = length . upText . unparsePara

getParaEditChar :: EditingPara c b -> Int
getParaEditChar (EditingPara _ l _ n) = n + getLineCursor l

setParaEditChar :: Int -> EditingPara c b -> EditingPara c b
setParaEditChar k p
  | getParaEditChar p > k && not (atParaFront p) = setParaEditChar k $ moveParaCursor MovePrev p
  | getParaEditChar p < k && not (atParaBack p)  = setParaEditChar k $ moveParaCursor MoveNext p
  | otherwise = p

splitPara :: FixedFontParser a c => a -> EditingPara c (BreakType a) -> (UnparsedPara c,UnparsedPara c)
splitPara parser (EditingPara bs l as _) = let (b,a) = splitLineAtCursor (splitLine parser) l in
  (unparseParaBefore $ VisibleParaBefore (b:bs) 0,
   unparseParaAfter  $ VisibleParaAfter  (a:as))

paraCursorMovable :: MoveDirection -> EditingPara c b -> Bool
paraCursorMovable d
  | d == MoveUp   = not . atParaTop
  | d == MoveDown = not . atParaBottom
  | d == MovePrev = not . atParaFront
  | d == MoveNext = not . atParaBack
  | d == MoveHome || d == MoveEnd = const True
  | otherwise = const False

moveParaCursor :: MoveDirection -> EditingPara c b -> EditingPara c b
moveParaCursor d p@(EditingPara bs l as n) = revised where
  revised
    | d == MoveHome || d == MoveEnd   = EditingPara bs (moveLineCursor d l)        as n
    | d == MoveUp   && atParaTop p    = EditingPara bs (moveLineCursor MoveUp l)   as n
    | d == MoveDown && atParaBottom p = EditingPara bs (moveLineCursor MoveDown l) as n
    | not (paraCursorMovable d p) = p
    | d == MoveUp   =
      setParaCursorChar (getLineCursor l) $
      EditingPara (tail bs) (editLine $ head bs) (viewLine l:as) (n-length (vlText $ head bs))
    | d == MoveDown =
      setParaCursorChar (getLineCursor l) $
      EditingPara (viewLine l:bs) (editLine $ head as) (tail as) (n+length (vlText $ viewLine l))
    | lineCursorMovable d l = EditingPara bs (moveLineCursor d l) as n
    | d == MovePrev = setBack  $ moveParaCursor MoveUp   p
    | d == MoveNext = setFront $ moveParaCursor MoveDown p
  setBack  (EditingPara bs l as n) = (EditingPara bs (moveLineCursor MoveDown l) as n)
  setFront (EditingPara bs l as n) = (EditingPara bs (moveLineCursor MoveUp   l) as n)

atParaFront :: EditingPara c b -> Bool
atParaFront p@(EditingPara _ l _ _) = atParaTop p && atLineFront l

atParaBack :: EditingPara c b -> Bool
atParaBack p@(EditingPara _ l _ _) = atParaBottom p && atLineBack l

seekParaFront :: EditingPara c b -> EditingPara c b
seekParaFront p
  | atParaFront p = p
  | otherwise     = seekParaFront $ moveParaCursor MoveUp p

seekParaBack :: EditingPara c b -> EditingPara c b
seekParaBack p
  | atParaBack p = p
  | otherwise    = seekParaBack $ moveParaCursor MoveDown p

appendToPara :: FixedFontParser a c
  => a -> EditingPara c (BreakType a) -> VisibleParaAfter c (BreakType a) -> EditingPara c (BreakType a)
appendToPara parser (EditingPara bs l as n) (VisibleParaAfter cs) = reparseParaTail parser revised where
  revised = EditingPara bs l (as ++ cs) n

prependToPara :: FixedFontParser a c
  => a -> VisibleParaBefore c (BreakType a) -> EditingPara c (BreakType a) -> EditingPara c (BreakType a)
prependToPara parser (VisibleParaBefore cs _) (EditingPara bs l as _) = reparseParaTail parser revised where
  revised = EditingPara bs2 l2 as n2
  (VisibleParaBefore bs' n') = parseParaBefore parser $ unparseParaBefore $ visibleParaBefore (bs ++ cs)
  (l2,bs2,n2) = if null bs'
                   then (l,[],0)
                   else (head bs' `prependToLine` l,tail bs',n'-length (vlText $ head bs'))

modifyPara :: FixedFontParser a c
  => a -> EditAction c -> EditDirection -> EditingPara c (BreakType a) -> EditingPara c (BreakType a)
modifyPara parser m d p = reparseParaTail parser revised where
  (EditingPara bs l as n) = mergeForEdit p
  revised = EditingPara bs (modifyLine m d l) as n


-- Private below here.

reparseParaTail :: FixedFontParser a c
  => a -> EditingPara c (BreakType a) -> EditingPara c (BreakType a)
reparseParaTail parser p@(EditingPara bs l as n) = setParaEditChar offset revised where
  offset = getParaEditChar p
  revised = EditingPara bs (editLine line) after n
  (line:after) = breakLines parser $ joinLines (viewLine l:as)

mergeForEdit :: EditingPara c b -> EditingPara c b
mergeForEdit (EditingPara bs l as n) = EditingPara bs2 l2 as2 n2 where
  l2 = addAfter as $ addBefore bs l where
    addAfter (v:_) l = l `appendToLine` v
    addAfter _ l = l
    addBefore (v:_) l = v `prependToLine` l
    addBefore _ l = l
  bs2 = if null bs then [] else tail bs
  as2 = if null as then [] else tail as
  n2 = n - (if null bs then 0 else length (vlText $ head bs))

atParaTop :: EditingPara c b -> Bool
atParaTop (EditingPara bs _ _ _) = null bs

atParaBottom :: EditingPara c b -> Bool
atParaBottom (EditingPara _ _ as _) = null as
