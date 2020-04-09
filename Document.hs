{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Safe #-}

module Document (
  EditingDocument,
  editDocument,
  flattenDocument,
) where

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
joinParaNext d@(EditingDocument _ _ [] _ _ _ _) = d
joinParaNext (EditingDocument bs e (a:as) w h k p) =
  EditingDocument bs (appendToPara p e a) as w h k p

joinParaPrev :: EditingDocument c -> EditingDocument c
joinParaPrev d@(EditingDocument [] _ _ _ _ _ _) = d
joinParaPrev (EditingDocument (b:bs) e as w h k p) =
  EditingDocument bs (prependToPara p b e) as w h (max 0 (k-1)) p

getVisibleLines :: EditingDocument c -> [VisibleLine c]
getVisibleLines (EditingDocument bs e as _ h k _) = bs2 ++ [e2] ++ as2 where
  bs2 = takeLinesBefore (min (h-1) k) (getBeforeLines e:bs)
  e2 = getCurrentLine e
  as2 = takeLinesAfter (h-k-1) (getAfterLines e:as)

resizeWidth :: Int -> EditingDocument c -> EditingDocument c
resizeWidth w (EditingDocument bs e as _ h k p) = (EditingDocument bs2 e2 as2 w h k p2) where
  p2 = setLineWidth p w
  bs2 = map (parseParaBefore p2 . unparseParaBefore p2) bs
  as2 = map (parseParaAfter  p2 . unparseParaAfter  p2) as
  e2 = reparsePara p2 e

resizeHeight :: Int -> EditingDocument c -> EditingDocument c
resizeHeight h2 (EditingDocument bs e as w h k p) =
  (EditingDocument bs e as w h2 (max 0 $ min (h2-1) k) p)
