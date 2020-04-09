{-# LANGUAGE Safe #-}

module Edit.Line (
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


data EditingLine c b =
  EditingLine {
    elTextBefore :: [c],  -- Reversed.
    elTextAfter :: [c],
    elCursor :: Int,
    elBreak :: b
  }
  deriving (Show)

editLine :: VisibleLine c b -> EditingLine c b
editLine (VisibleLine cs b) = EditingLine [] cs 0 b

viewLine :: EditingLine c b -> VisibleLine c b
viewLine (EditingLine bs as _ b) = VisibleLine (reverse bs ++ as) b

getLineCursor :: EditingLine c b -> Int
getLineCursor = elCursor

setLineCursor :: Int -> EditingLine c b -> EditingLine c b
setLineCursor k e@(EditingLine bs as c b)
  | k < c && not (null bs) = setLineCursor k $ EditingLine (tail bs) (head bs:as) (c-1) b
  | k > c && not (null as) = setLineCursor k $ EditingLine (head as:bs) (tail as) (c+1) b
  | otherwise = e

splitLine :: Enum b => EditingLine c b -> (VisibleLine c b,VisibleLine c b)
splitLine (EditingLine bs as c b) =
  (VisibleLine (reverse bs) (toEnum 0),
   VisibleLine as b)

lineCursorMovable :: MoveDirection -> EditingLine c b -> Bool
lineCursorMovable MovePrev (EditingLine (_:_) _ _ _) = True
lineCursorMovable MoveNext (EditingLine _ (_:_) _ _) = True
lineCursorMovable _ _ = False

moveLineCursor :: MoveDirection -> EditingLine c b -> EditingLine c b
moveLineCursor MovePrev (EditingLine bs as c b)
  | not (null bs) = (EditingLine (tail bs) (head bs:as) (c-1) b)
moveLineCursor MoveNext (EditingLine bs as c b)
  | not (null as) = (EditingLine (head as:bs) (tail as) (c+1) b)
moveLineCursor _ l = l

atLineFront :: EditingLine c b -> Bool
atLineFront = null . elTextBefore

atLineBack :: EditingLine c b -> Bool
atLineBack = null . elTextAfter

setCursorFront :: EditingLine c b -> EditingLine c b
setCursorFront (EditingLine bs as c b) = (EditingLine [] (reverse bs ++ as) 0 b)

setCursorBack :: EditingLine c b -> EditingLine c b
setCursorBack (EditingLine bs as c b) = (EditingLine (reverse as ++ bs) [] (length as + length bs) b)

appendToLine :: EditingLine c b -> VisibleLine c b -> EditingLine c b
appendToLine (EditingLine bs as c b) (VisibleLine cs _) =
  (EditingLine bs (as ++ cs) c b)

prependToLine :: VisibleLine c b -> EditingLine c b -> EditingLine c b
prependToLine (VisibleLine cs b) (EditingLine bs as c _) =
  (EditingLine (bs ++ reverse cs) as (c + length cs) b)

modifyLine :: EditAction c -> EditDirection -> EditingLine c b -> EditingLine c b
modifyLine (InsertText cs) d (EditingLine bs as c b) = revised where
  bs2 = if d == EditBefore
           then reverse cs ++ bs
           else bs
  as2 = if d == EditAfter
           then cs ++ as
           else as
  revised = EditingLine bs2 as2 (length bs2) b
modifyLine DeleteText d (EditingLine bs as c b) = revised where
  bs2 = if d == EditBefore
           then tail bs
           else bs
  as2 = if d == EditAfter
           then tail as
           else as
  revised = EditingLine bs2 as2 (length bs2) b
