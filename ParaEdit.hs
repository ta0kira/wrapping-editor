{-# LANGUAGE Safe #-}

module ParaEdit (
  EditingPara,
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
  viewParaAfter,
  viewParaBefore,
) where

import Actions
import Line
import LineEdit
import Para
import Parser


data EditingPara c =
  EditingPara {
    epBefore :: [VisibleLine c],  -- Reversed.
    epEditing :: EditingLine c,
    epAfter :: [VisibleLine c]
  }
  deriving (Show)

parseParaBefore :: FixedFontParser a c => a -> UnparsedPara c -> VisibleParaBefore c
parseParaBefore parser (UnparsedPara cs) = VisibleParaBefore (reverse $ breakLines parser cs)

parseParaAfter :: FixedFontParser a c => a -> UnparsedPara c -> VisibleParaAfter c
parseParaAfter parser (UnparsedPara cs) = VisibleParaAfter (breakLines parser cs)

unparseParaBefore :: FixedFontParser a c => a -> VisibleParaBefore c -> UnparsedPara c
unparseParaBefore parser (VisibleParaBefore ls) = UnparsedPara $ joinLines parser (reverse ls)

unparseParaAfter :: FixedFontParser a c => a -> VisibleParaAfter c -> UnparsedPara c
unparseParaAfter parser (VisibleParaAfter ls) = UnparsedPara $ joinLines parser ls

editPara :: FixedFontParser a c => a -> UnparsedPara c -> EditingPara c
editPara parser (UnparsedPara cs) = EditingPara [] (editLine line) after where
  (line:after) = nonempty $ breakLines parser cs
  nonempty [] = [emptyLine]
  nonempty ls = ls

unparsePara :: FixedFontParser a c => a -> EditingPara c -> UnparsedPara c
unparsePara parser (EditingPara bs l as) = UnparsedPara $ joinLines parser ls where
  ls = reverse bs ++ [viewLine l] ++ as

reparsePara :: FixedFontParser a c => a -> EditingPara c -> EditingPara c
reparsePara parser (EditingPara bs l as) = reparseParaTail parser revised where
  revised = EditingPara bs2 l2 as
  bs' = breakLines parser $ joinLines parser (reverse bs)
  (l2,bs2)
    | null bs' = (l,[])
    | otherwise = (head bs' `prependToLine` l,tail bs')

viewParaBefore :: EditingPara c -> VisibleParaBefore c
viewParaBefore (EditingPara bs l as) = VisibleParaBefore ls where
  ls = reverse bs ++ [viewLine l] ++ as

viewParaAfter :: EditingPara c -> VisibleParaAfter c
viewParaAfter (EditingPara bs l as) = VisibleParaAfter ls where
  ls = reverse as ++ [viewLine l] ++ bs

getBeforeLines :: EditingPara c -> VisibleParaBefore c
getBeforeLines = VisibleParaBefore . epBefore

getCurrentLine :: EditingPara c -> VisibleLine c
getCurrentLine = viewLine . epEditing

getAfterLines :: EditingPara c -> VisibleParaAfter c
getAfterLines = VisibleParaAfter . epAfter

takeLinesBefore :: Int -> [VisibleParaBefore c] -> [VisibleLine c]
takeLinesBefore n = reverse . take n . concat . map vpbLines

takeLinesAfter :: Int -> [VisibleParaAfter c] -> [VisibleLine c]
takeLinesAfter n = take n . concat . map vpaLines

getParaCursor :: EditingPara c -> Int
getParaCursor = getLineCursor . epEditing

getCursorLine :: EditingPara c -> Int
getCursorLine = length . epBefore

setParaCursor :: Int -> EditingPara c -> EditingPara c
setParaCursor k e@(EditingPara bs l as) = (EditingPara bs (setLineCursor k l) as)

splitPara :: EditingPara c -> (VisibleParaBefore c,VisibleParaAfter c)
splitPara (EditingPara bs l as) =
  (VisibleParaBefore bs,VisibleParaAfter (viewLine l:as))

paraCursorMovable :: MoveDirection -> EditingPara c -> Bool
paraCursorMovable d (EditingPara bs l as)
  | d == MoveUp   = not (null bs)
  | d == MoveDown = not (null as)
  | otherwise = lineCursorMovable d l

moveParaCursor :: MoveDirection -> EditingPara c -> EditingPara c
moveParaCursor d p@(EditingPara bs l as) = revised where
  revised
    | not (paraCursorMovable d p) = p
    | d == MoveUp   = setParaCursor (getLineCursor l) $ EditingPara (tail bs) (editLine $ head bs) (viewLine l:as)
    | d == MoveDown = setParaCursor (getLineCursor l) $ EditingPara (viewLine l:bs) (editLine $ head as) (tail as)
    | lineCursorMovable d l = EditingPara bs (moveLineCursor d l) as
    | d == MovePrev = setBack  $ moveParaCursor MoveUp   p
    | d == MoveNext = setFront $ moveParaCursor MoveDown p
  setBack  (EditingPara bs l as) = (EditingPara bs (setCursorBack  l) as)
  setFront (EditingPara bs l as) = (EditingPara bs (setCursorFront l) as)

atParaFront :: EditingPara c -> Bool
atParaFront (EditingPara bs l _) = null bs && atLineFront l

atParaBack :: EditingPara c -> Bool
atParaBack (EditingPara _ l as) = null as && atLineBack l

seekParaFront :: EditingPara c -> EditingPara c
seekParaFront (EditingPara [] l as) = EditingPara [] (setCursorFront l) as
seekParaFront (EditingPara bs l as) =
  seekParaFront $ EditingPara [] (editLine $ last bs) (reverse (init bs) ++ [viewLine l] ++ as)

seekParaBack :: EditingPara c -> EditingPara c
seekParaBack (EditingPara bs l []) = EditingPara bs (setCursorBack l) []
seekParaBack (EditingPara bs l as) =
  seekParaBack $ EditingPara (reverse (init as) ++ [viewLine l] ++ bs) (editLine $ last as) []

appendToPara :: FixedFontParser a c => a -> EditingPara c -> VisibleParaAfter c -> EditingPara c
appendToPara parser p (VisibleParaAfter []) = p
appendToPara parser (EditingPara bs l as) (VisibleParaAfter (c:cs)) = reparseParaTail parser revised where
  revised = EditingPara bs (l `appendToLine` c) (as ++ cs)

prependToPara :: FixedFontParser a c => a -> VisibleParaBefore c -> EditingPara c -> EditingPara c
prependToPara parser (VisibleParaBefore []) p = p
prependToPara parser (VisibleParaBefore (c:cs)) (EditingPara bs l as) = reparseParaTail parser revised where
  revised = EditingPara (cs ++ bs) (c `prependToLine` l) as

modifyPara :: FixedFontParser a c => a -> EditAction c -> EditDirection -> EditingPara c -> EditingPara c
modifyPara parser m@(InsertText _) d (EditingPara bs l as) =
  reparseParaTail parser (EditingPara bs (modifyLine m d l) as)
modifyPara parser m@(DeleteText _) d p = reparseParaTail parser revised where
  (EditingPara bs l as) = mergeForDelete parser p
  revised = (EditingPara bs (modifyLine m d l) as)


-- Private below here.

reparseParaTail :: FixedFontParser a c => a -> EditingPara c -> EditingPara c
reparseParaTail parser (EditingPara bs l as) = moveBy offset revised where
  offset = getLineCursor l
  revised = EditingPara bs (editLine line) after
  (line:after) = breakLines parser $ joinLines parser (viewLine l:as)
  moveBy k
    | k > 0 = moveBy (k-1) . moveParaCursor MoveNext
    | otherwise = id

mergeForDelete :: FixedFontParser a c => a -> EditingPara c -> EditingPara c
mergeForDelete parser (EditingPara bs l as) = EditingPara bs2 l2 as2 where
  l2 = addAfter as $ addBefore bs l where
    addAfter (v:_) l = l `appendToLine` v
    addAfter _ l = l
    addBefore (v:_) l = v `prependToLine` l
    addBefore _ l = l
  bs2 = if null bs then [] else tail bs
  as2 = if null as then [] else tail as
