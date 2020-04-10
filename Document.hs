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

{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Safe #-}

module Document (
  EditingDocument,
  editDocument,
  exportDocument,
  -- From Base >>>
  EditAction(..),
  EditDirection(..),
  EditorAction,
  FixedFontEditor(..),
  FixedFontParser,
  FixedFontViewer(..),
  MoveDirection(..),
  UnparsedPara(..),
  ViewerAction,
  editorAppendAction,
  editorBackspaceAction,
  editorDeleteAction,
  editorDownAction,
  editorEnterAction,
  editorLeftAction,
  editorInsertAction,
  editorRightAction,
  editorUpAction,
  viewerResizeAction,
  -- <<< From Base
) where

import Base.Actions
import Base.Editor
import Base.Line
import Base.Para
import Base.Parser
import Base.Viewer
import Edit.Line
import Edit.Para


data EditingDocument c b =
  forall a. FixedFontParser a c b => EditingDocument {
    edBefore :: [VisibleParaBefore c b],  -- Reversed.
    edEditing :: EditingPara c b,
    edAfter :: [VisibleParaAfter c b],
    edWidth :: Int,
    edHeight :: Int,
    edOffset :: Int,
    edLastCursor :: Int,
    edParser :: a
  }

instance FixedFontViewer (EditingDocument c b) c where
  setViewSize d s@(w,h)
    | w /= edWidth d && h /= edHeight d = storeCursor $ resizeWidth w $ resizeHeight h d
    | w /= edWidth d  = storeCursor $ resizeWidth  w d
    | h /= edHeight d = storeCursor $ resizeHeight h d
    | otherwise = d
  getViewSize d = (edWidth d,edHeight d)
  getVisible d@(EditingDocument _ _ _ _ _ _ _ p) = map (renderLine p) $ getVisibleLines d

instance FixedFontEditor (EditingDocument c b) c where
  editText da m d = storeCursor $ modifyDoc m d da
  breakPara da d = storeCursor $ insertParaSplit d da
  moveCursor da d = updateCursor $ moveDocCursor d da where
    updateCursor
      | d == MoveUp || d == MoveDown = applyCursor
      | otherwise = storeCursor
  getCursor (EditingDocument _ e _ _ _ k _ _) = (getParaCursor e,k)

editDocument :: FixedFontParser a c b => a -> [UnparsedPara c] -> EditingDocument c b
editDocument parser ps = document where
  document = EditingDocument {
      edBefore = [],
      edEditing = editPara parser first,
      edAfter = map (parseParaAfter parser) rest,
      edWidth = 0,
      edHeight = 0,
      edOffset = 0,
      edLastCursor = 0,
      edParser = parser
    }
  (first:rest) = nonempty ps
  nonempty [] = [emptyPara]
  nonempty ps = ps

exportDocument :: EditingDocument c b -> [UnparsedPara c]
exportDocument (EditingDocument bs e as _ _ _ _ p) =
  reverse (map (unparseParaBefore p) bs) ++ [unparsePara p e] ++ (map (unparseParaAfter p) as)


-- Private below here.

storeCursor :: EditingDocument c b -> EditingDocument c b
storeCursor (EditingDocument bs e as w h k _ p) =
  EditingDocument bs e as w h k (getParaCursor e) p

applyCursor :: EditingDocument c b -> EditingDocument c b
applyCursor (EditingDocument bs e as w h k c p) =
  EditingDocument bs (setParaCursor c e) as w h k c p

joinParaNext :: EditingDocument c b -> EditingDocument c b
joinParaNext da@(EditingDocument _ _ [] _ _ _ _ _) = da
joinParaNext (EditingDocument bs e (a:as) w h k c p) =
  EditingDocument bs (appendToPara p e a) as w h k c p

joinParaPrev :: EditingDocument c b -> EditingDocument c b
joinParaPrev da@(EditingDocument [] _ _ _ _ _ _ _) = da
joinParaPrev (EditingDocument (b:bs) e as w h k c p) =
  EditingDocument bs (prependToPara p b e) as w h (max 0 (k-1)) c p

getVisibleLines :: EditingDocument c b -> [VisibleLine c b]
getVisibleLines (EditingDocument bs e as _ h k _ _) = bs2 ++ [e2] ++ as2 where
  bs2 = takeLinesBefore (min (h-1) k) (getBeforeLines e:bs)
  e2 = getCurrentLine e
  as2 = takeLinesAfter (h-length bs2-1) (getAfterLines e:as)

resizeWidth :: Int -> EditingDocument c b -> EditingDocument c b
resizeWidth w (EditingDocument bs e as _ h k c p) = (EditingDocument bs2 e2 as2 w h k c p2) where
  p2 = setLineWidth p w
  bs2 = map (parseParaBefore p2 . unparseParaBefore p2) bs
  as2 = map (parseParaAfter  p2 . unparseParaAfter  p2) as
  e2 = reparsePara p2 e

resizeHeight :: Int -> EditingDocument c b -> EditingDocument c b
resizeHeight h2 (EditingDocument bs e as w _ k c p) =
  (EditingDocument bs e as w h2 (max 0 $ min (h2-1) k) c p)

moveDocCursor :: MoveDirection -> EditingDocument c b -> EditingDocument c b
moveDocCursor d da@(EditingDocument bs e as w h k c p) = revised where
  revised
    | paraCursorMovable d e =
      let e2 = moveParaCursor d e in (EditingDocument bs e2 as w h (fixOffset e2) c p)
    | d == MoveUp && null bs =
      let e2 = seekParaFront e in (EditingDocument bs e2 as w h k c p)
    | d == MoveUp && not (null bs) =
        let
          bs2 = tail bs
          e2 = seekParaBack $ editPara p $ unparseParaBefore p $ head bs
          as2 = viewParaAfter e:as
          k2 = max 0 (k-1) in
        -- NOTE: This doesn't preserve cursor position.
        EditingDocument bs2 e2 as2 w h k2 c p
    | d == MoveDown && null as =
      let e2 = seekParaBack e in (EditingDocument bs e2 as w h k c p)
    | d == MoveDown && not (null as) =
        let
          bs2 = viewParaBefore e:bs
          e2 = editPara p $ unparseParaAfter p $ head as
          as2 = tail as
          k2 = min (h-1) (k+1) in
        -- NOTE: This doesn't preserve cursor position.
        EditingDocument bs2 e2 as2 w h k2 c p
    | d == MovePrev = seekBack  $ moveDocCursor MoveUp   da
    | d == MoveNext = seekFront $ moveDocCursor MoveDown da
  fixOffset e2 = min (h-1) $ max 0 $ k + (getCursorLine e2 - getCursorLine e)
  seekBack  (EditingDocument bs e as w h k c p) = EditingDocument bs (seekParaBack e)  as w h k c p
  seekFront (EditingDocument bs e as w h k c p) = EditingDocument bs (seekParaFront e) as w h k c p

modifyDoc :: EditAction c -> EditDirection -> EditingDocument c b -> EditingDocument c b
modifyDoc m d da@(EditingDocument bs e as w h k c p) = revised m d where
  revised DeleteText EditBefore
    | atParaFront e && not (null bs) =
      EditingDocument (tail bs) (prependToPara p (head bs) e) as w h (max 0 (k-1)) c p
  revised DeleteText EditAfter
    | atParaBack e && not (null as) =
      EditingDocument bs (appendToPara p e (head as)) (tail as) w h (min (h-1) (k+1)) c p
  revised _ _ = EditingDocument bs (modifyPara p m d e) as w h k c p

insertParaSplit :: EditDirection -> EditingDocument c b -> EditingDocument c b
insertParaSplit d (EditingDocument bs e as w h k c p) = revised where
  (b,a) = splitPara p e
  revised
    | d == EditBefore = EditingDocument (parseParaBefore p b:bs) (editPara p a) as w h (min (h-1) (k+1)) c p
    | d == EditAfter  = EditingDocument bs (seekParaBack $ editPara p b) (parseParaAfter p a:as) w h k c p
