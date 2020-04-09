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


data LineBreakType = ParaBreak | SpaceBreak | BrokenWord | TokenBreak deriving (Enum,Eq,Ord,Show)

instance FixedFontParser ParsePolicy Char LineBreakType where
  setLineWidth _ w = BreakExact w
  breakParas _ = map UnparsedPara . lines
  joinParas _ = unlines . map upText
  breakLines _ [] = [emptyLine]
  breakLines b@(BreakExact w) cs
    | w < 2 = [VisibleLine cs ParaBreak]
    | otherwise = correct (take w cs) (drop w cs) where
      break2 [] = []
      break2 ys = breakLines b ys
      correct xs ys
        | last xs == ' ' =
          -- Drop space from end of line.
          (VisibleLine (init xs) SpaceBreak):(break2 ys)
      correct xs [] = [VisibleLine xs ParaBreak]
      correct xs ys
        | head ys == ' ' =
          -- Drop space from beginning of next line.
          (VisibleLine xs SpaceBreak):(break2 (tail ys))
      correct xs ys
        | isLetter (last xs) && isLetter (head ys) =
          fixWord (init xs) (last xs:ys)
      correct xs ys = (VisibleLine xs TokenBreak):(break2 ys)
      -- TODO: This won't work properly when left/right cursor movement crosses lines.
      fixWord xs ys
        | isLetter (last xs) && isLetter (head ys) =
          (VisibleLine xs BrokenWord):(break2 ys)
        | last xs == ' ' =
          -- Drop space from end of line.
          (VisibleLine (init xs) SpaceBreak):(break2 ys)
        | otherwise = (VisibleLine xs TokenBreak):(break2 ys)
  joinLines _ = concat . map fixLine where
    fixLine (VisibleLine cs SpaceBreak) = cs ++ " "
    fixLine (VisibleLine cs _) = cs
  renderLine _ (VisibleLine cs BrokenWord) = cs ++ "-"
  renderLine _ (VisibleLine cs _) = cs

main = do
  contents <- readFile "Test/testdata.txt"
  let doc = setViewSize (editDocument breakExact contents) (26,13)
  putStr $ unlines $ getVisible doc
  putStrLn $ show $ flattenDocument doc == contents
