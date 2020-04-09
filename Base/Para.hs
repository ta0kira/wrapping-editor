{-# LANGUAGE Safe #-}

module Base.Para (
  UnparsedPara(..),
  emptyPara,
) where

import Base.Line


data UnparsedPara c =
  UnparsedPara {
    upText :: [c]
  }
  deriving (Eq,Ord,Show)

emptyPara :: UnparsedPara c
emptyPara = UnparsedPara []
