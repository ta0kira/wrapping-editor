{-# LANGUAGE Safe #-}

module Para (
  StaticPara(..),
) where

import Line


data StaticPara c =
  UnparsedPara {
    upText :: [c]
  } |
  VisibleParaBefore {
    vpaLines :: [VisibleLine c]  -- Reversed.
  } |
  VisibleParaAfter {
    vpbLines :: [VisibleLine c]
  }
  deriving (Show)
