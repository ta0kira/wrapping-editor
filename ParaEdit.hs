{-# LANGUAGE Safe #-}

module ParaEdit (
  EditingPara,
  VisiblePara,
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
import Line
import LineEdit
import Para
import Parser


data VisiblePara c =
  VisibleParaBefore {
    vpaLines :: [VisibleLine c]  -- Reversed.
  } |
  VisibleParaAfter {
    vpbLines :: [VisibleLine c]
  }
  deriving (Show)

data EditingPara c =
  EditingPara {
    epBefore :: [VisibleLine c],  -- Reversed.
    epEditing :: EditingLine c,
    epAfter :: [VisibleLine c],
    epLine :: Int,
    epHeight :: Int
  }
  deriving (Show)

parseParaBefore :: FixedFontParser a c => a -> UnparsedPara c -> VisiblePara c
parseParaBefore parser (UnparsedPara cs) = VisibleParaBefore (reverse $ breakLines parser cs)

parseParaAfter :: FixedFontParser a c => a -> UnparsedPara c -> VisiblePara c
parseParaAfter parser (UnparsedPara cs) = VisibleParaAfter (breakLines parser cs)

unparsePara :: FixedFontParser a c => a -> VisiblePara c -> UnparsedPara c
unparsePara parser (VisibleParaBefore cs) = UnparsedPara (joinLines parser $ reverse cs)
unparsePara parser (VisibleParaAfter cs)  = UnparsedPara (joinLines parser cs)

editPara :: FixedFontParser a c => a -> UnparsedPara c -> EditingPara c
editPara parser (UnparsedPara cs) = EditingPara [] (editLine line) after 0 (length after + 1) where
  (line:after) = nonempty $ breakLines parser cs
  nonempty [] = [emptyLine]
  nonempty ls = ls

viewPara :: FixedFontParser a c => a -> EditingPara c -> VisiblePara c
viewPara parser (EditingPara bs l as _ _) = VisibleParaAfter ls where
  ls = reverse bs ++ [viewLine l] ++ as

getParaCursor :: EditingPara c -> (Int,Int)
getParaCursor (EditingPara _ l _ n _) = (getLineCursor l,n)

setParaCursor :: Int -> EditingPara c -> EditingPara c
setParaCursor k e@(EditingPara bs l as n h) = (EditingPara bs (setLineCursor k l) as n h)

splitPara :: EditingPara c -> (VisiblePara c,VisiblePara c)
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
