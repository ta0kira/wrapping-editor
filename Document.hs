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
    edHeight :: Int,
    edOffset :: Int,
    edParser :: a
  }

editDocument :: FixedFontParser a c => a -> [c] -> EditingDocument c
editDocument parser cs = document where
  document = EditingDocument {
      edBefore = [],
      edEditing = editPara parser first,
      edAfter = map (parseParaAfter parser) rest,
      edHeight = 0,
      edOffset = 0,
      edParser = parser
    }
  (first:rest) = nonempty (breakParas parser cs)
  nonempty [] = [emptyPara]
  nonempty ps = ps

flattenDocument :: EditingDocument c -> [c]
flattenDocument (EditingDocument bs e as _ _ p) = joinParas p ps where
  ps = reverse (map (unparseParaBefore p) bs) ++ [unparsePara p e] ++ (map (unparseParaAfter p) as)


-- Private below here.

joinParaNext :: EditingDocument c -> EditingDocument c
joinParaNext d@(EditingDocument _ _ [] _ _ _) = d
joinParaNext (EditingDocument bs e (a:as) h k p) =
  EditingDocument bs (appendToPara p e a) as h k p

joinParaPrev :: EditingDocument c -> EditingDocument c
joinParaPrev d@(EditingDocument [] _ _ _ _ _) = d
joinParaPrev (EditingDocument (b:bs) e as h k p) =
  EditingDocument bs (prependToPara p b e) as h (max 0 (k-1)) p

getVisibleLines :: EditingDocument c -> [VisibleLine c]
getVisibleLines (EditingDocument bs e as h k _) = bs2 ++ [e2] ++ as2 where
  bs2 = takeLinesBefore (min (h-1) k) (getBeforeLines e:bs)
  e2 = getCurrentLine e
  as2 = takeLinesAfter (h-k-1) (getAfterLines e:as)
