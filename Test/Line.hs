{- -----------------------------------------------------------------------------
Copyright 2020 Kevin P. Barry

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
----------------------------------------------------------------------------- -}

-- Author: Kevin P. Barry [ta0kira@gmail.com]

{-# LANGUAGE Safe #-}

module Test.Line (
  allTests,
) where

import LineWrap
import Base.Actions
import Base.Line
import Edit.Line
import Test.Common


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
           moveLineCursor MoveHome,
           modifyLine (InsertText "X") EditAfter
         ]),
    ("insert at back", checkLineEdit
       "this is a test line"
       "this is a test lineX" $
       composeActions [
           setLineCursor 5,
           moveLineCursor MoveEnd,
           modifyLine (InsertText "X") EditAfter
         ]),
    ("delete after front", checkLineEdit
       "this is a test line"
       "his is a test line" $
       composeActions [
           setLineCursor 5,
           moveLineCursor MoveHome,
           modifyLine DeleteText EditAfter
         ]),
    ("delete before front", checkLineEdit
       "this is a test line"
       "this is a test line" $
       composeActions [
           setLineCursor 5,
           moveLineCursor MoveHome,
           modifyLine DeleteText EditBefore
         ]),
    ("delete before back", checkLineEdit
       "this is a test line"
       "this is a test lin" $
       composeActions [
           setLineCursor 5,
           moveLineCursor MoveEnd,
           modifyLine DeleteText EditBefore
         ]),
    ("delete after back", checkLineEdit
       "this is a test line"
       "this is a test line" $
       composeActions [
           setLineCursor 5,
           moveLineCursor MoveEnd,
           modifyLine DeleteText EditAfter
         ]),
    ("split line middle", do
       let line = editLine $ innerLine "this is a test line"
       let (left,right) = splitLine $ setLineCursor 5 line
       checkConditions [
           ("Left: " ++ show left,left == (endLine "this ")),
           ("Right: " ++ show right,right == (innerLine "is a test line"))
         ]),
    ("split line front", do
       let line = editLine $ innerLine "this is a test line"
       let (left,right) = splitLine $ moveLineCursor MoveUp line
       checkConditions [
           ("Left: " ++ show left,left == (endLine "")),
           ("Right: " ++ show right,right == (innerLine "this is a test line"))
         ]),
    ("split line back", do
       let line = editLine $ innerLine "this is a test line"
       let (left,right) = splitLine $ moveLineCursor MoveDown line
       checkConditions [
           ("Left: " ++ show left,left == (endLine "this is a test line")),
           ("Right: " ++ show right,right == (innerLine ""))
         ]),
    ("prepend preserves cursor", checkLineEdit
       "this is a test line"
       "XYthis Zis a test line" $
       composeActions [
           setLineCursor 5,
           prependToLine (innerLine "XY"),
           modifyLine (InsertText "Z") EditAfter
         ]),
    ("append preserves cursor", checkLineEdit
       "this is a test line"
       "this Zis a test lineXY" $
       composeActions [
           setLineCursor 5,
           flip appendToLine (endLine "XY"),
           modifyLine (InsertText "Z") EditAfter
         ])
  ]

checkLineEdit x y f = do
  let lx = endLine x
  let ly = endLine y
  let edit = f (editLine lx)
  let restored = viewLine edit
  checkCondition (show restored) (restored == ly)
