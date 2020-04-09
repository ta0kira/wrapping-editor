{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE Safe #-}

module Document (
  EditingDocument,
) where

import Line
import LineEdit
import Para
import ParaEdit
import Parser


data EditingDocument c =
  forall a. FixedFontParser a c => EditingDocument {
    edBefore :: [VisiblePara c],  -- Reversed.
    edEditing :: EditingPara c,
    edAfter :: [VisiblePara c],
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
