{-# LANGUAGE Safe #-}

module Line (
  LineBreakType(..),
  VisibleLine(..),
) where


data LineBreakType = ParaBreak | SpaceBreak | BrokenWord | TokenBreak deriving (Eq,Ord,Show)

data VisibleLine c =
  VisibleLine {
    vlText :: [c],
    vlWidth :: Int,
    vlBreak :: LineBreakType
  }
  deriving (Show)
