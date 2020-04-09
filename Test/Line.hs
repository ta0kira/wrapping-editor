{-# LANGUAGE Safe #-}

module Test.Line (
  allTests,
) where

import Actions
import Line
import Edit.Line
import Test.Base


allTests :: [(String,IO (Maybe String))]
allTests = [
    ("edit preserves line", checkLineEdit
       "this is a test line"
       "this is a test line"
       id),
    ("bounded cursor move preserves line", checkLineEdit
       "this is a test line"
       "this is a test line" $
       repeatAction 5 (moveLineCursor MoveNext)),
    ("undershot cursor move preserves line", checkLineEdit
       "this is a test line"
       "this is a test line" $
       repeatAction 100 (moveLineCursor MovePrev)),
    ("overshot cursor move preserves line", checkLineEdit
       "this is a test line"
       "this is a test line" $
       repeatAction 100 (moveLineCursor MoveNext)),
    ("bounded cursor seek preserves line", checkLineEdit
       "this is a test line"
       "this is a test line" $
       setLineCursor 5),
    ("undershot cursor seek preserves line", checkLineEdit
       "this is a test line"
       "this is a test line" $
       setLineCursor (-5)),
    ("overshot cursor seek preserves line", checkLineEdit
       "this is a test line"
       "this is a test line" $
       setLineCursor 100),
    ("insert before", checkLineEdit
       "this is a test line"
       "this XYis a test line" $
       composeActions [
           setLineCursor 5,
           modifyLine (InsertText "X") EditBefore,
           modifyLine (InsertText "Y") EditBefore
         ]),
    ("insert after", checkLineEdit
       "this is a test line"
       "this YXis a test line" $
       composeActions [
           setLineCursor 5,
           modifyLine (InsertText "X") EditAfter,
           modifyLine (InsertText "Y") EditAfter
         ]),
    ("delete before", checkLineEdit
       "this is a test line"
       "thiis a test line" $
       composeActions [
           setLineCursor 5,
           modifyLine DeleteText EditBefore,
           modifyLine DeleteText EditBefore
         ]),
    ("delete after", checkLineEdit
       "this is a test line"
       "this  a test line" $
       composeActions [
           setLineCursor 5,
           modifyLine DeleteText EditAfter,
           modifyLine DeleteText EditAfter
         ]),
    ("insert at front", checkLineEdit
       "this is a test line"
       "Xthis is a test line" $
       composeActions [
           setLineCursor 5,
           moveLineCursor MoveUp,
           modifyLine (InsertText "X") EditAfter
         ]),
    ("insert at back", checkLineEdit
       "this is a test line"
       "this is a test lineX" $
       composeActions [
           setLineCursor 5,
           moveLineCursor MoveDown,
           modifyLine (InsertText "X") EditAfter
         ]),
    ("delete after front", checkLineEdit
       "this is a test line"
       "his is a test line" $
       composeActions [
           setLineCursor 5,
           moveLineCursor MoveUp,
           modifyLine DeleteText EditAfter
         ]),
    ("delete before front", checkLineEdit
       "this is a test line"
       "this is a test line" $
       composeActions [
           setLineCursor 5,
           moveLineCursor MoveUp,
           modifyLine DeleteText EditBefore
         ]),
    ("delete before back", checkLineEdit
       "this is a test line"
       "this is a test lin" $
       composeActions [
           setLineCursor 5,
           moveLineCursor MoveDown,
           modifyLine DeleteText EditBefore
         ]),
    ("delete after back", checkLineEdit
       "this is a test line"
       "this is a test line" $
       composeActions [
           setLineCursor 5,
           moveLineCursor MoveDown,
           modifyLine DeleteText EditAfter
         ]),
    ("split line middle", do
       let line = editLine $ VisibleLine "this is a test line" AlternateBreak
       let (left,right) = splitLine $ setLineCursor 5 line
       checkConditions [
           (left == (VisibleLine "this " LineBreak),"Left: " ++ show left),
           (right == (VisibleLine "is a test line" AlternateBreak),"Right: " ++ show right)
         ]),
    ("split line front", do
       let line = editLine $ VisibleLine "this is a test line" AlternateBreak
       let (left,right) = splitLine $ moveLineCursor MoveUp line
       checkConditions [
           (left == (VisibleLine "" LineBreak),"Left: " ++ show left),
           (right == (VisibleLine "this is a test line" AlternateBreak),"Right: " ++ show right)
         ]),
    ("split line back", do
       let line = editLine $ VisibleLine "this is a test line" AlternateBreak
       let (left,right) = splitLine $ moveLineCursor MoveDown line
       checkConditions [
           (left == (VisibleLine "this is a test line" LineBreak),"Left: " ++ show left),
           (right == (VisibleLine "" AlternateBreak),"Right: " ++ show right)
         ]),
    ("prepend preserves cursor", checkLineEdit
       "this is a test line"
       "XYthis Zis a test line" $
       composeActions [
           setLineCursor 5,
           prependToLine (newLine "XY"),
           modifyLine (InsertText "Z") EditAfter
         ]),
    ("append preserves cursor", checkLineEdit
       "this is a test line"
       "this Zis a test lineXY" $
       composeActions [
           setLineCursor 5,
           flip appendToLine (newLine "XY"),
           modifyLine (InsertText "Z") EditAfter
         ])
  ]

checkLineEdit x y f = do
  let lx = newLine x
  let ly = newLine y
  let edit = f (editLine lx)
  let restored = viewLine edit
  checkCondition (restored == ly) (show restored)
