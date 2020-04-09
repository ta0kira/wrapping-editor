{-# LANGUAGE Safe #-}

module Document (
  EditingDocument,
) where

import Line
import Para


data EditingDocument c =
  EditingDocument {
    edBefore :: [StaticPara c],  -- Reversed.
    edEditing :: EditingPara c,
    edAfter :: [StaticPara c],
    edWidth :: Int,
    edHeight :: Int,
    edViewOffset :: Int
  }
  deriving (Show)
