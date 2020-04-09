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

data VisibleParaBefore c b =
  VisibleParaBefore {
    vpbLines :: [VisibleLine c b]  -- Reversed.
  }
  deriving (Show)

data VisibleParaAfter c b =
  VisibleParaAfter {
    vpaLines :: [VisibleLine c b]
  }
  deriving (Show)
