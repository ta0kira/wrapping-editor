{-# LANGUAGE Safe #-}

module Base.Line (
  VisibleLine(..),
  emptyLine,
) where


data VisibleLine c b =
  VisibleLine {
    vlText :: [c],
    vlBreak :: b
  }
  deriving (Eq,Ord,Show)

emptyLine :: Enum b => VisibleLine c b
emptyLine = VisibleLine [] (toEnum 0)
