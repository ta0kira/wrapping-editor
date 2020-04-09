{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Safe #-}

import Data.Char (isLetter)

import Document
import Editor
import Line
import Para
import Parser
import Viewer


data ParsePolicy = BreakExact Int deriving (Show)

breakExact = BreakExact 0

instance FixedFontParser ParsePolicy Char where
  setLineWidth _ w = BreakExact w
  breakParas _ = map UnparsedPara . lines
  joinParas _ = unlines . map upText
  breakLines _ [] = [emptyLine]
  breakLines b@(BreakExact w) cs
    | w < 2 = [VisibleLine cs (length cs) ParaBreak]
    | otherwise = correct (take w cs) (drop w cs) where
      break2 [] = []
      break2 ys = breakLines b ys
      correct xs ys
        | last xs == ' ' =
          -- Drop space from end of line.
          (VisibleLine (init xs) (length xs-1) SpaceBreak):(break2 ys)
      correct xs [] = [VisibleLine xs (length xs) ParaBreak]
      correct xs ys
        | head ys == ' ' =
          -- Drop space from beginning of next line.
          (VisibleLine xs (length xs) SpaceBreak):(break2 (tail ys))
      correct xs ys
        | isLetter (last xs) && isLetter (head ys) =
          fixWord (init xs) (last xs:ys)
      correct xs ys = (VisibleLine xs (length xs) TokenBreak):(break2 ys)
      fixWord xs ys
        | isLetter (last xs) && isLetter (head ys) =
          (VisibleLine xs (length xs) BrokenWord):(break2 ys)
        | last xs == ' ' =
          -- Drop space from end of line.
          (VisibleLine (init xs) (length xs-1) SpaceBreak):(break2 ys)
        | otherwise = (VisibleLine xs (length xs) TokenBreak):(break2 ys)
  joinLines _ = concat . map fixLine where
    fixLine (VisibleLine cs _ SpaceBreak) = cs ++ " "
    fixLine (VisibleLine cs _ _) = cs
  renderLine _ (VisibleLine cs _ BrokenWord) = cs ++ "-"
  renderLine _ (VisibleLine cs _ _) = cs

main = do
  contents <- readFile "testdata.txt"
  let doc = setViewSize (editDocument breakExact contents) (26,13)
  putStr $ unlines $ getVisible doc
  putStrLn $ show $ flattenDocument doc == contents
