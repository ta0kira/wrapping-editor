{-# LANGUAGE Safe #-}

module Line (
  LineBreakType(..),
  VisibleLine(..),
  emptyLine,
) where


data LineBreakType = ParaBreak | SpaceBreak | BrokenWord | TokenBreak deriving (Eq,Ord,Show)

data VisibleLine c =
  VisibleLine {
    vlText :: [c],
    vlWidth :: Int,
    vlBreak :: LineBreakType
  }
  deriving (Show)

emptyLine :: VisibleLine c
emptyLine = VisibleLine [] 0 ParaBreak
