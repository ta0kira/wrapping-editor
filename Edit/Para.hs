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

{-# LANGUAGE Safe #-}

module Edit.Para (
  EditingPara,
  VisibleParaAfter,
  VisibleParaBefore,
  appendToPara,
  atParaBack,
  atParaFront,
  editPara,
  getAfterLines,
  getBeforeLines,
  getCurrentLine,
  getCursorLine,
  getParaCursor,
  modifyPara,
  moveParaCursor,
  paraCursorMovable,
  parseParaAfter,
  parseParaBefore,
  prependToPara,
  reparsePara,
  seekParaBack,
  seekParaFront,
  setParaCursor,
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

import Base.Actions
import Base.Line
import Base.Para
import Base.Parser
import Edit.Line


data VisibleParaBefore c b =
  VisibleParaBefore {
    vpbLines :: [VisibleLine c b]  -- Reversed.
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
    epAfter :: [VisibleLine c b]
  }
  deriving (Show)

viewBeforeLines :: VisibleParaBefore c b -> [VisibleLine c b]
viewBeforeLines = reverse . vpbLines

viewAfterLines :: VisibleParaAfter c b -> [VisibleLine c b]
viewAfterLines = vpaLines

parseParaBefore :: FixedFontParser a c b => a -> UnparsedPara c -> VisibleParaBefore c b
parseParaBefore parser (UnparsedPara cs) = VisibleParaBefore $ reverse $ breakLines parser cs

parseParaAfter :: FixedFontParser a c b => a -> UnparsedPara c -> VisibleParaAfter c b
parseParaAfter parser (UnparsedPara cs) = VisibleParaAfter $ breakLines parser cs

unparseParaBefore :: FixedFontParser a c b => a -> VisibleParaBefore c b -> UnparsedPara c
unparseParaBefore parser (VisibleParaBefore ls) = UnparsedPara $ joinLines parser $ reverse ls

unparseParaAfter :: FixedFontParser a c b => a -> VisibleParaAfter c b -> UnparsedPara c
unparseParaAfter parser (VisibleParaAfter ls) = UnparsedPara $ joinLines parser ls

editPara :: FixedFontParser a c b => a -> UnparsedPara c -> EditingPara c b
editPara parser (UnparsedPara cs) = EditingPara [] (editLine line) after where
  (line:after) = nonempty $ breakLines parser cs
  nonempty [] = [emptyLine]
  nonempty ls = ls

unparsePara :: FixedFontParser a c b => a -> EditingPara c b -> UnparsedPara c
unparsePara parser (EditingPara bs l as) = UnparsedPara $ joinLines parser ls where
  ls = reverse bs ++ [viewLine l] ++ as

reparsePara :: FixedFontParser a c b => a -> EditingPara c b -> EditingPara c b
reparsePara parser (EditingPara bs l as) = reparseParaTail parser revised where
  revised = EditingPara bs2 l2 as
  bs' = reverse $ breakLines parser $ joinLines parser (reverse bs)
  (l2,bs2)
    | null bs' = (l,[])
    | otherwise = (head bs' `prependToLine` l,tail bs')

viewParaBefore :: EditingPara c b -> VisibleParaBefore c b
viewParaBefore (EditingPara bs l as) = VisibleParaBefore ls where
  ls = reverse $ reverse bs ++ [viewLine l] ++ as

viewParaAfter :: EditingPara c b -> VisibleParaAfter c b
viewParaAfter (EditingPara bs l as) = VisibleParaAfter ls where
  ls = reverse bs ++ [viewLine l] ++ as

getBeforeLines :: EditingPara c b -> VisibleParaBefore c b
getBeforeLines = VisibleParaBefore . epBefore

getCurrentLine :: EditingPara c b -> VisibleLine c b
getCurrentLine = viewLine . epEditing

getAfterLines :: EditingPara c b -> VisibleParaAfter c b
getAfterLines = VisibleParaAfter . epAfter

takeLinesBefore :: Int -> [VisibleParaBefore c b] -> [VisibleLine c b]
takeLinesBefore n = reverse . take n . concat . map vpbLines

takeLinesAfter :: Int -> [VisibleParaAfter c b] -> [VisibleLine c b]
takeLinesAfter n = take n . concat . map vpaLines

getParaCursor :: EditingPara c b -> Int
getParaCursor = getLineCursor . epEditing

getCursorLine :: EditingPara c b -> Int
getCursorLine = length . epBefore

setParaCursor :: Int -> EditingPara c b -> EditingPara c b
setParaCursor k e@(EditingPara bs l as) = (EditingPara bs (setLineCursor k l) as)

splitPara :: FixedFontParser a c b => a -> EditingPara c b -> (UnparsedPara c,UnparsedPara c)
splitPara parser (EditingPara bs l as) = let (b,a) = splitLine l in
  (unparseParaBefore parser $ VisibleParaBefore (b:bs),
   unparseParaAfter  parser $ VisibleParaAfter  (a:as))

paraCursorMovable :: MoveDirection -> EditingPara c b -> Bool
paraCursorMovable d
  | d == MoveUp   = not . atParaTop
  | d == MoveDown = not . atParaBottom
  | d == MovePrev = not . atParaFront
  | d == MoveNext = not . atParaBack

moveParaCursor :: MoveDirection -> EditingPara c b -> EditingPara c b
moveParaCursor d p@(EditingPara bs l as) = revised where
  revised
    | not (paraCursorMovable d p) = p
    | d == MoveUp   = setParaCursor (getLineCursor l) $ EditingPara (tail bs) (editLine $ head bs) (viewLine l:as)
    | d == MoveDown = setParaCursor (getLineCursor l) $ EditingPara (viewLine l:bs) (editLine $ head as) (tail as)
    | lineCursorMovable d l = EditingPara bs (moveLineCursor d l) as
    | d == MovePrev = setBack  $ moveParaCursor MoveUp   p
    | d == MoveNext = setFront $ moveParaCursor MoveDown p
  setBack  (EditingPara bs l as) = (EditingPara bs (moveLineCursor MoveDown l) as)
  setFront (EditingPara bs l as) = (EditingPara bs (moveLineCursor MoveUp   l) as)

atParaFront :: EditingPara c b -> Bool
atParaFront p@(EditingPara _ l _) = atParaTop p && atLineFront l

atParaBack :: EditingPara c b -> Bool
atParaBack p@(EditingPara _ l _) = atParaBottom p && atLineBack l

seekParaFront :: EditingPara c b -> EditingPara c b
seekParaFront (EditingPara [] l as) = EditingPara [] (moveLineCursor MoveUp l) as
seekParaFront (EditingPara bs l as) =
  seekParaFront $ EditingPara [] (editLine $ last bs) (reverse (init bs) ++ [viewLine l] ++ as)

seekParaBack :: EditingPara c b -> EditingPara c b
seekParaBack (EditingPara bs l []) = EditingPara bs (moveLineCursor MoveDown l) []
seekParaBack (EditingPara bs l as) =
  seekParaBack $ EditingPara (reverse (init as) ++ [viewLine l] ++ bs) (editLine $ last as) []

appendToPara :: FixedFontParser a c b => a -> EditingPara c b -> VisibleParaAfter c b -> EditingPara c b
appendToPara parser (EditingPara bs l as) (VisibleParaAfter cs) = reparseParaTail parser revised where
  revised = EditingPara bs l (as ++ cs)

prependToPara :: FixedFontParser a c b => a -> VisibleParaBefore c b -> EditingPara c b -> EditingPara c b
prependToPara parser (VisibleParaBefore cs) (EditingPara bs l as) = reparseParaTail parser revised where
  revised = EditingPara bs2 l2 as
  bs' = vpbLines $ parseParaBefore parser $ unparseParaBefore parser $ VisibleParaBefore (bs ++ cs)
  (l2,bs2) = if null bs'
                then (l,[])
                else (head bs' `prependToLine` l,tail bs')

modifyPara :: FixedFontParser a c b => a -> EditAction c -> EditDirection -> EditingPara c b -> EditingPara c b
modifyPara parser m d p = reparseParaTail parser revised where
  (EditingPara bs l as) = mergeForEdit parser p
  revised = (EditingPara bs (modifyLine m d l) as)


-- Private below here.

reparseParaTail :: FixedFontParser a c b => a -> EditingPara c b -> EditingPara c b
reparseParaTail parser (EditingPara bs l as) = moveBy offset revised where
  offset = getLineCursor l
  revised = EditingPara bs (editLine line) after
  (line:after) = breakLines parser $ joinLines parser (viewLine l:as)
  moveBy k e
    | k > 0 = moveBy (next k e) $ moveParaCursor MoveNext e
    | otherwise = e
  -- An extra move is required when crossing lines.
  next k (EditingPara _ l _) = k - (if atLineBack l then 0 else 1)

mergeForEdit :: FixedFontParser a c b => a -> EditingPara c b -> EditingPara c b
mergeForEdit parser (EditingPara bs l as) = EditingPara bs2 l2 as2 where
  l2 = addAfter as $ addBefore bs l where
    addAfter (v:_) l = l `appendToLine` v
    addAfter _ l = l
    addBefore (v:_) l = v `prependToLine` l
    addBefore _ l = l
  bs2 = if null bs then [] else tail bs
  as2 = if null as then [] else tail as

atParaTop :: EditingPara c b -> Bool
atParaTop (EditingPara bs _ _) = null bs

atParaBottom :: EditingPara c b -> Bool
atParaBottom (EditingPara _ _ as) = null as
