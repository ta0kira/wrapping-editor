{-# LANGUAGE Safe #-}

module LineEdit (
  EditingLine,
  appendToLine,
  atLineBack,
  atLineFront,
  editLine,
  emptyLine,
  getLineCursor,
  lineCursorMovable,
  modifyLine,
  moveLineCursor,
  prependToLine,
  setCursorBack,
  setCursorFront,
  setLineCursor,
  splitLine,
  viewLine,
) where

import Actions
import Line


data EditingLine c =
  EditingLine {
    elTextBefore :: [c],  -- Reversed.
    elTextAfter :: [c],
    elCursor :: Int,
    elWidth :: Int,
    elBreak :: LineBreakType
  }
  deriving (Show)


editLine :: VisibleLine c -> EditingLine c
editLine (VisibleLine cs w b) = EditingLine [] cs 0 w b

viewLine :: EditingLine c -> VisibleLine c
viewLine (EditingLine bs as _ w b) = VisibleLine (reverse bs ++ as) w b

getLineCursor :: EditingLine c -> Int
getLineCursor = elCursor

setLineCursor :: Int -> EditingLine c -> EditingLine c
setLineCursor k e@(EditingLine bs as c w b)
  | k < c && not (null bs) = setLineCursor k $ EditingLine (tail bs) (head bs:as) (c-1) w b
  | k > c && not (null as) = setLineCursor k $ EditingLine (head as:bs) (tail as) (c+1) w b
  | otherwise = e

splitLine :: EditingLine c -> (VisibleLine c,VisibleLine c)
splitLine (EditingLine bs as c w b) =
  (VisibleLine (reverse bs) (length bs) ParaBreak,
   VisibleLine as (length as) b)

lineCursorMovable :: MoveDirection -> EditingLine c -> Bool
lineCursorMovable MovePrev (EditingLine (_:_) _ _ _ _) = True
lineCursorMovable MoveNext (EditingLine _ (_:_) _ _ _) = True
lineCursorMovable _ _ = False

moveLineCursor :: MoveDirection -> EditingLine c -> EditingLine c
moveLineCursor MovePrev (EditingLine bs as c w b)
  | not (null bs) = (EditingLine (tail bs) (head bs:as) (c-1) w b)
moveLineCursor MoveNext (EditingLine bs as c w b)
  | not (null as) = (EditingLine (head as:bs) (tail as) (c+1) w b)
moveLineCursor _ l = l

atLineFront :: EditingLine c -> Bool
atLineFront l = elCursor l == 0

atLineBack :: EditingLine c -> Bool
atLineBack l = elCursor l == elWidth l

setCursorFront :: EditingLine c -> EditingLine c
setCursorFront (EditingLine bs as c w b) = (EditingLine [] (reverse bs ++ as) 0 w b)

setCursorBack :: EditingLine c -> EditingLine c
setCursorBack (EditingLine bs as c w b) = (EditingLine (reverse as ++ bs) [] w w b)

appendToLine :: EditingLine c -> VisibleLine c -> EditingLine c
appendToLine (EditingLine bs as c w b) (VisibleLine cs w2 _) =
  (EditingLine bs (as ++ cs) c (w+w2) b)

prependToLine :: VisibleLine c -> EditingLine c -> EditingLine c
prependToLine (VisibleLine cs w2 b) (EditingLine bs as c w _) =
  (EditingLine (bs ++ reverse cs) as (c+w2) (w+w2) b)

modifyLine :: EditAction c -> EditDirection -> EditingLine c -> EditingLine c
modifyLine (InsertText cs) d (EditingLine bs as c w b) = revised where
  bs2 = if d == EditBefore
           then reverse cs ++ bs
           else bs
  as2 = if d == EditAfter
           then cs ++ as
           else as
  revised = EditingLine bs2 as2 (length bs2) (length bs2 + length as2) b
modifyLine DeleteText d (EditingLine bs as c w b) = revised where
  bs2 = if d == EditBefore
           then tail bs
           else bs
  as2 = if d == EditAfter
           then tail as
           else as
  revised = EditingLine bs2 as2 (length bs2) (length bs2 + length as2) b
