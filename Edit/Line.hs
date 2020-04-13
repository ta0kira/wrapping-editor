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

{-# LANGUAGE Safe #-}

module Edit.Line (
  EditingLine,
  appendToLine,
  atLineBack,
  atLineFront,
  editLine,
  getLineCursor,
  joinLines,
  lineCursorMovable,
  modifyLine,
  moveLineCursor,
  prependToLine,
  setLineCursor,
  splitLine,
  viewLine,
) where

import Base.Actions
import Base.Line


data EditingLine c b =
  EditingLine {
    elTextBefore :: [c],  -- Reversed.
    elTextAfter :: [c],
    elCursor :: Int,
    elBreak :: b
  }
  deriving (Eq,Show)

editLine :: VisibleLine c b -> EditingLine c b
editLine (VisibleLine b cs) = EditingLine [] cs 0 b

viewLine :: EditingLine c b -> VisibleLine c b
viewLine (EditingLine bs as _ b) = VisibleLine b (reverse bs ++ as)

joinLines :: [VisibleLine c b] -> [c]
joinLines = concat . map vlText

getLineCursor :: EditingLine c b -> Int
getLineCursor = elCursor

setLineCursor :: Int -> EditingLine c b -> EditingLine c b
setLineCursor k e@(EditingLine bs as c b)
  | k < c && not (null bs) = setLineCursor k $ EditingLine (tail bs) (head bs:as) (c-1) b
  | k > c && not (null as) = setLineCursor k $ EditingLine (head as:bs) (tail as) (c+1) b
  | otherwise = e

splitLine :: DefaultBreak b => EditingLine c b -> (VisibleLine c b,VisibleLine c b)
splitLine (EditingLine bs as c b) =
  (VisibleLine defaultBreak (reverse bs),
   VisibleLine b as)

lineCursorMovable :: MoveDirection -> EditingLine c b -> Bool
lineCursorMovable MovePrev (EditingLine (_:_) _ _ _) = True
lineCursorMovable MoveNext (EditingLine _ (_:_) _ _) = True
lineCursorMovable MoveHome _ = True
lineCursorMovable MoveEnd  _ = True
lineCursorMovable _ _ = False

moveLineCursor :: MoveDirection -> EditingLine c b -> EditingLine c b
moveLineCursor MoveHome p = moveLineCursor MoveUp   p
moveLineCursor MoveEnd  p = moveLineCursor MoveDown p
moveLineCursor MoveUp (EditingLine bs as c b) =
  (EditingLine [] (reverse bs ++ as) 0 b)
moveLineCursor MoveDown (EditingLine bs as c b) =
  (EditingLine (reverse as ++ bs) [] (length as + length bs) b)
moveLineCursor MovePrev (EditingLine bs as c b)
  | not (null bs) = (EditingLine (tail bs) (head bs:as) (c-1) b)
moveLineCursor MoveNext (EditingLine bs as c b)
  | not (null as) = (EditingLine (head as:bs) (tail as) (c+1) b)
moveLineCursor _ l = l

atLineFront :: EditingLine c b -> Bool
atLineFront = null . elTextBefore

atLineBack :: EditingLine c b -> Bool
atLineBack = null . elTextAfter

appendToLine :: EditingLine c b -> VisibleLine c b -> EditingLine c b
appendToLine (EditingLine bs as c _) (VisibleLine b cs) =
  (EditingLine bs (as ++ cs) c b)

prependToLine :: VisibleLine c b -> EditingLine c b -> EditingLine c b
prependToLine (VisibleLine _ cs) (EditingLine bs as c b) =
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
  bs2 = if d == EditBefore && not (null bs)
           then tail bs
           else bs
  as2 = if d == EditAfter && not (null as)
           then tail as
           else as
  revised = EditingLine bs2 as2 (length bs2) b
