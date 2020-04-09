{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Safe #-}

module Document (
  EditingDocument,
  editDocument,
  flattenDocument,
) where

import Actions
import Editor
import Line
import LineEdit
import Para
import ParaEdit
import Parser
import Viewer


data EditingDocument c =
  forall a. FixedFontParser a c => EditingDocument {
    edBefore :: [VisibleParaBefore c],  -- Reversed.
    edEditing :: EditingPara c,
    edAfter :: [VisibleParaAfter c],
    edWidth :: Int,
    edHeight :: Int,
    edOffset :: Int,
    edParser :: a
  }

instance FixedFontViewer (EditingDocument c) c where
  setViewSize d s@(w,h)
    | w /= edWidth d  = flip setViewSize s $ resizeWidth  w d
    | h /= edHeight d = flip setViewSize s $ resizeHeight h d
    | otherwise = d
  getViewSize d = (edWidth d,edHeight d)
  getVisible = getVisibleLines

instance FixedFontEditor (EditingDocument c) c where
  editText da m d = modifyDoc m d da
  breakPara da d = insertParaSplit d da
  moveCursor da d = moveDocCursor d da
  getCursor (EditingDocument _ e _ _ _ k _) = (getParaCursor e,k)

editDocument :: FixedFontParser a c => a -> [c] -> EditingDocument c
editDocument parser cs = document where
  document = EditingDocument {
      edBefore = [],
      edEditing = editPara parser first,
      edAfter = map (parseParaAfter parser) rest,
      edWidth = 0,
      edHeight = 0,
      edOffset = 0,
      edParser = parser
    }
  (first:rest) = nonempty (breakParas parser cs)
  nonempty [] = [emptyPara]
  nonempty ps = ps

flattenDocument :: EditingDocument c -> [c]
flattenDocument (EditingDocument bs e as _ _ _ p) = joinParas p ps where
  ps = reverse (map (unparseParaBefore p) bs) ++ [unparsePara p e] ++ (map (unparseParaAfter p) as)


-- Private below here.

joinParaNext :: EditingDocument c -> EditingDocument c
joinParaNext da@(EditingDocument _ _ [] _ _ _ _) = da
joinParaNext (EditingDocument bs e (a:as) w h k p) =
  EditingDocument bs (appendToPara p e a) as w h k p

joinParaPrev :: EditingDocument c -> EditingDocument c
joinParaPrev da@(EditingDocument [] _ _ _ _ _ _) = da
joinParaPrev (EditingDocument (b:bs) e as w h k p) =
  EditingDocument bs (prependToPara p b e) as w h (max 0 (k-1)) p

getVisibleLines :: EditingDocument c -> [VisibleLine c]
getVisibleLines (EditingDocument bs e as _ h k _) = bs2 ++ [e2] ++ as2 where
  bs2 = takeLinesBefore (min (h-1) k) (getBeforeLines e:bs)
  e2 = getCurrentLine e
  as2 = takeLinesAfter (h-length bs2-1) (getAfterLines e:as)

resizeWidth :: Int -> EditingDocument c -> EditingDocument c
resizeWidth w (EditingDocument bs e as _ h k p) = (EditingDocument bs2 e2 as2 w h k p2) where
  p2 = setLineWidth p w
  bs2 = map (parseParaBefore p2 . unparseParaBefore p2) bs
  as2 = map (parseParaAfter  p2 . unparseParaAfter  p2) as
  e2 = reparsePara p2 e

resizeHeight :: Int -> EditingDocument c -> EditingDocument c
resizeHeight h2 (EditingDocument bs e as w h k p) =
  (EditingDocument bs e as w h2 (max 0 $ min (h2-1) k) p)

moveDocCursor :: MoveDirection -> EditingDocument c -> EditingDocument c
moveDocCursor d da@(EditingDocument bs e as w h k p) = revised where
  revised
    | paraCursorMovable d e = let e2 = moveParaCursor d e in (EditingDocument bs e2 as w h (fixOffset e2) p)
    | d == MoveUp && not (null bs) =
        let
          bs2 = tail bs
          e2 = editPara p $ unparseParaBefore p $ head bs
          as2 = viewParaAfter e:as
          k2 = max 0 (k-1) in
        EditingDocument bs2 e2 as2 w h k2 p
    | d == MoveDown && not (null as) =
        let
          bs2 = viewParaBefore e:bs
          e2 = editPara p $ unparseParaAfter p $ head as
          as2 = tail as
          k2 = min (h-1) (k+1) in
        EditingDocument bs2 e2 as2 w h k2 p
    | d == MovePrev = seekBack  $ moveDocCursor MoveUp   da
    | d == MoveNext = seekFront $ moveDocCursor MoveDown da
  fixOffset e2 = min (h-1) $ max 0 $ k + (getCursorLine e2 - getCursorLine e)
  seekBack  (EditingDocument bs e as w h k p) = EditingDocument bs (seekParaBack e)  as w h k p
  seekFront (EditingDocument bs e as w h k p) = EditingDocument bs (seekParaFront e) as w h k p

modifyDoc :: EditAction c -> EditDirection -> EditingDocument c -> EditingDocument c
modifyDoc m d da@(EditingDocument bs e as w h k p) = revised m d where
  revised (DeleteText _) EditBefore
    | atParaFront e && not (null bs) =
      EditingDocument (tail bs) (prependToPara p (head bs) e) as w h (max 0 (k-1)) p
  revised (DeleteText _) EditAfter
    | atParaBack e && not (null as) =
      EditingDocument bs (appendToPara p e (head as)) (tail as) w h (min (h-1) (k+1)) p
  revised _ _ = EditingDocument bs (modifyPara p m d e) as w h k p

insertParaSplit :: EditDirection -> EditingDocument c -> EditingDocument c
insertParaSplit d (EditingDocument bs e as w h k p) = revised where
  (b,a) = splitPara p e
  revised
    | d == EditBefore = EditingDocument (parseParaBefore p b:bs) (editPara p a) as w h k p
    | d == EditAfter  = EditingDocument bs (editPara p b) (parseParaAfter p a:as) w h k p
