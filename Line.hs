{-# LANGUAGE Safe #-}

module Line (
  VisibleLine(..),
  emptyLine,
) where


data VisibleLine c b =
  VisibleLine {
    vlText :: [c],
    vlWidth :: Int,
    vlBreak :: b
  }
  deriving (Show)

emptyLine :: Enum b => VisibleLine c b
emptyLine = VisibleLine [] 0 (toEnum 0)
