{-# LANGUAGE Safe #-}

module ParaEdit (
  StaticPara(..),  -- From Para.
  EditingPara,
  editPara,
  getParaCursor,
  modifyPara,
  moveParaCursor,
  paraCursorMovable,
  parseParaAfter,
  parseParaBefore,
  setParaCursor,
  splitPara,
  viewPara,
) where

import Actions
import LineEdit
import Para
import Parser


data EditingPara c =
  EditingPara {
    epBefore :: [VisibleLine c],  -- Reversed.
    epEditing :: EditingLine c,
    epAfter :: [VisibleLine c],
    epLine :: Int,
    epHeight :: Int
  }
  deriving (Show)

parseParaBefore :: FixedFontParser a c => a -> StaticPara c -> StaticPara c
parseParaBefore parser para = VisibleParaBefore (reverse $ breakLines parser cs) where
  cs = flattenPara parser para

parseParaAfter :: FixedFontParser a c => a -> StaticPara c -> StaticPara c
parseParaAfter parser para = VisibleParaBefore (breakLines parser cs) where
  cs = flattenPara parser para

editPara :: FixedFontParser a c => a -> StaticPara c -> EditingPara c
editPara parser para = EditingPara [] (editLine line) after 0 (length after + 1) where
  (line:after) = nonempty $ breakLines parser $ flattenPara parser para
  nonempty [] = [emptyLine]
  nonempty ls = ls

viewPara :: FixedFontParser a c => a -> EditingPara c -> StaticPara c
viewPara parser (EditingPara bs l as _ _) = VisibleParaAfter ls where
  ls = reverse bs ++ [viewLine l] ++ as

getParaCursor :: EditingPara c -> (Int,Int)
getParaCursor (EditingPara _ l _ n _) = (getLineCursor l,n)

setParaCursor :: Int -> EditingPara c -> EditingPara c
setParaCursor k e@(EditingPara bs l as n h) = (EditingPara bs (setLineCursor k l) as n h)

splitPara :: EditingPara c -> (StaticPara c,StaticPara c)
splitPara (EditingPara bs l as _ _) =
  (VisibleParaBefore bs,VisibleParaAfter (viewLine l:as))

paraCursorMovable :: MoveDirection -> EditingPara c -> Bool
paraCursorMovable d (EditingPara bs l as _ _)
  | d == MoveUp   = not (null bs)
  | d == MoveDown = not (null as)
  | otherwise = lineCursorMovable d l

moveParaCursor :: MoveDirection -> EditingPara c -> EditingPara c
moveParaCursor d p@(EditingPara bs l as n h) = revised where
  revised
    | not (paraCursorMovable d p) = p
    | d == MoveUp   = setParaCursor (getLineCursor l) $ EditingPara (tail bs) (editLine $ head bs) (viewLine l:as) (n-1) h
    | d == MoveDown = setParaCursor (getLineCursor l) $ EditingPara (viewLine l:bs) (editLine $ head as) (tail as) (n+1) h
    | lineCursorMovable d l = EditingPara bs (moveLineCursor d l) as n h
    | d == MovePrev = setBack  $ moveParaCursor MoveUp   p
    | d == MoveNext = setFront $ moveParaCursor MoveDown p
  setBack  (EditingPara bs l as n h) = (EditingPara bs (setCursorBack  l) as n h)
  setFront (EditingPara bs l as n h) = (EditingPara bs (setCursorFront l) as n h)
  h2 = h - (if null bs then 0 else 1) - (if null as then 0 else 1)

modifyPara :: FixedFontParser a c => a -> EditAction c -> EditDirection -> EditingPara c -> EditingPara c
modifyPara parser m@(InsertText _) d (EditingPara bs l as n h) =
  reparseParaTail parser (EditingPara bs (modifyLine m d l) as n h)
modifyPara parser m@(DeleteText _) d p = reparseParaTail parser revised where
  (EditingPara bs l as n h) = mergeForDelete parser p
  revised = (EditingPara bs (modifyLine m d l) as n h)


-- Private below here.

flattenPara :: FixedFontParser a c => a -> StaticPara c -> [c]
flattenPara parser (UnparsedPara cs) = cs
flattenPara parser (VisibleParaBefore ls) = reverse $ joinLines parser ls
flattenPara parser (VisibleParaAfter ls) = joinLines parser ls

reparseParaTail :: FixedFontParser a c => a -> EditingPara c -> EditingPara c
reparseParaTail parser (EditingPara bs l as n h) = moveBy offset revised where
  offset = getLineCursor l
  revised = EditingPara bs (editLine line) after n (n + length as + 1)
  (line:after) = breakLines parser $ joinLines parser (viewLine l:as)
  moveBy k
    | k > 0 = moveBy (k-1) . moveParaCursor MoveNext
    | otherwise = id

mergeForDelete :: FixedFontParser a c => a -> EditingPara c -> EditingPara c
mergeForDelete parser (EditingPara bs l as n h) = EditingPara bs2 l2 as2 n2 h2 where
  l2 = addAfter as $ addBefore bs l where
    addAfter (v:_) l = appendToLine v l
    addAfter _ l = l
    addBefore (v:_) l = prependToLine v l
    addBefore _ l = l
  bs2 = if null bs then [] else tail bs
  as2 = if null as then [] else tail as
  n2 = n - (if null bs then 0 else 1)
  h2 = h - (if null bs then 0 else 1) - (if null as then 0 else 1)
