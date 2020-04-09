{-# LANGUAGE ExistentialQuantification #-}
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


data EditingDocument c =
  forall a. FixedFontParser a c => EditingDocument {
    edBefore :: [VisibleParaBefore c],  -- Reversed.
    edEditing :: EditingPara c,
    edAfter :: [VisibleParaAfter c],
    edWidth :: Int,
    edHeight :: Int,
    edViewOffset :: Int,
    edParser :: a
  }

editDocument :: FixedFontParser a c => a -> [c] -> EditingDocument c
editDocument parser cs = document where
  document = EditingDocument {
      edBefore = [],
      edEditing = editPara parser first,
      edAfter = map (parseParaAfter parser) rest,
      edWidth = 0,
      edHeight = 0,
      edViewOffset = 0,
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
joinParaNext (EditingDocument bs e (a:as) w h o p) =
  EditingDocument bs (appendToPara p e a) as w h o p

joinParaPrev :: EditingDocument c -> EditingDocument c
joinParaPrev d@(EditingDocument [] _ _ _ _ _ _) = d
joinParaPrev (EditingDocument (b:bs) e as w h o p) =
  EditingDocument bs (prependToPara p b e) as w h (max 0 (o-1)) p
