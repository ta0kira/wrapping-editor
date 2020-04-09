{-# LANGUAGE Safe #-}

module Para (
  UnparsedPara(..),
  emptyPara,
) where

import Line


data UnparsedPara c =
  UnparsedPara {
    upText :: [c]
  }
  deriving (Eq,Ord,Show)

emptyPara :: UnparsedPara c
emptyPara = UnparsedPara []
