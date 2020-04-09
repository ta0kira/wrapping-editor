{-# LANGUAGE Safe #-}

module Para (
  UnparsedPara(..),
  VisibleParaAfter(..),
  VisibleParaBefore(..),
  emptyPara,
) where

import Line


data UnparsedPara c =
  UnparsedPara {
    upText :: [c]
  }
  deriving (Show)

emptyPara :: UnparsedPara c
emptyPara = UnparsedPara []

data VisibleParaBefore c =
  VisibleParaBefore {
    vpbLines :: [VisibleLine c]  -- Reversed.
  }
  deriving (Show)

data VisibleParaAfter c =
  VisibleParaAfter {
    vpaLines :: [VisibleLine c]
  }
  deriving (Show)
