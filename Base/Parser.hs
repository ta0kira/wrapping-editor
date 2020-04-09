{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Safe #-}

module Base.Parser (
  FixedFontParser(..),
) where

import Base.Line
import Base.Para


class Enum b => FixedFontParser a c b | a -> c b where
  setLineWidth :: a -> Int -> a
  breakParas :: a -> [c] -> [UnparsedPara c]
  joinParas :: a -> [UnparsedPara c] -> [c]
  breakLines :: a -> [c] -> [VisibleLine c b]
  joinLines :: a -> [VisibleLine c b] -> [c]
  renderLine :: a -> VisibleLine c b -> [c]
