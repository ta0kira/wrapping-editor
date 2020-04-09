{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Safe #-}

module Parser (
  FixedFontParser(..),
) where

import Line
import Para


class FixedFontParser a c | a -> c where
  setLineWidth :: a -> Int -> a
  breakParas :: a -> [c] -> [StaticPara c]
  joinParas :: a -> [StaticPara c] -> [c]
  breakLines :: a -> [c] -> [VisibleLine c]
  joinLines :: a -> [VisibleLine c] -> [c]
  renderLine :: a -> VisibleLine c -> [c]
