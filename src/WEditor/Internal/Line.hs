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

module WEditor.Internal.Line (
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
  splitLineAtCursor,
  viewLine,
) where

import WEditor.Base.Editor
import WEditor.Base.Line


data EditingLine c b =
  EditingLine {
    elTextBefore :: [c],  -- Reversed.
    elTextAfter :: [c],
    elBreak :: b
  }
  deriving (Eq,Show)

editLine :: VisibleLine c b -> EditingLine c b
editLine (VisibleLine b cs) = EditingLine [] cs b

viewLine :: EditingLine c b -> VisibleLine c b
viewLine (EditingLine bs as b) = VisibleLine b (reverse bs ++ as)

joinLines :: [VisibleLine c b] -> [c]
joinLines = concat . map vlText

getLineCursor :: EditingLine c b -> Int
getLineCursor = length . elTextBefore

setLineCursor :: Int -> EditingLine c b -> EditingLine c b
setLineCursor k e@(EditingLine bs as b) = EditingLine bs2 as2 b where
  (bs2,as2) = seek (length bs) bs as
  seek n bs as
    | k < n && not (null bs) = seek (n-1) (tail bs)    (head bs:as)
    | k > n && not (null as) = seek (n+1) (head as:bs) (tail as)
    | otherwise = (bs,as)

splitLineAtCursor :: (Int -> VisibleLine c b -> (VisibleLine c b,VisibleLine c b))
                  -> EditingLine c b -> (VisibleLine c b,VisibleLine c b)
splitLineAtCursor f l@(EditingLine bs _ _) = f (length bs) (viewLine l)

lineCursorMovable :: MoveDirection -> EditingLine c b -> Bool
lineCursorMovable MovePrev (EditingLine (_:_) _ _) = True
lineCursorMovable MoveNext (EditingLine _ (_:_) _) = True
lineCursorMovable MoveHome _ = True
lineCursorMovable MoveEnd  _ = True
lineCursorMovable _ _ = False

moveLineCursor :: MoveDirection -> EditingLine c b -> EditingLine c b
moveLineCursor MoveHome p = moveLineCursor MoveUp   p
moveLineCursor MoveEnd  p = moveLineCursor MoveDown p
moveLineCursor MoveUp (EditingLine bs as b) =
  (EditingLine [] (reverse bs ++ as) b)
moveLineCursor MoveDown (EditingLine bs as b) =
  (EditingLine (reverse as ++ bs) [] b)
moveLineCursor MovePrev (EditingLine bs as b)
  | not (null bs) = (EditingLine (tail bs) (head bs:as) b)
moveLineCursor MoveNext (EditingLine bs as b)
  | not (null as) = (EditingLine (head as:bs) (tail as) b)
moveLineCursor _ l = l

atLineFront :: EditingLine c b -> Bool
atLineFront = null . elTextBefore

atLineBack :: EditingLine c b -> Bool
atLineBack = null . elTextAfter

appendToLine :: EditingLine c b -> VisibleLine c b -> EditingLine c b
appendToLine (EditingLine bs as c) (VisibleLine b cs) =
  (EditingLine bs (as ++ cs) b)

prependToLine :: VisibleLine c b -> EditingLine c b -> EditingLine c b
prependToLine (VisibleLine _ cs) (EditingLine bs as b) =
  (EditingLine (bs ++ reverse cs) as b)

modifyLine :: EditAction c -> EditDirection -> EditingLine c b -> EditingLine c b
modifyLine (InsertText cs) d (EditingLine bs as b) = revised where
  bs2 = if d == EditBefore
           then reverse cs ++ bs
           else bs
  as2 = if d == EditAfter
           then cs ++ as
           else as
  revised = EditingLine bs2 as2 b
modifyLine DeleteText d (EditingLine bs as b) = revised where
  bs2 = if d == EditBefore && not (null bs)
           then tail bs
           else bs
  as2 = if d == EditAfter && not (null as)
           then tail as
           else as
  revised = EditingLine bs2 as2 b
