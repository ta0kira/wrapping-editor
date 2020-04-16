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

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Safe #-}

module TestDocument (
  allTests,
) where

import WEditor.Document
import WEditor.LineWrap

import Common


allTests :: [(String,IO (Maybe String))]
allTests = [
    ("document preserved", checkEditContent
       "testfiles/original-long.txt"
       "testfiles/original-long.txt"
       id),
    ("default view at top", checkEditView
       "testfiles/original-long.txt"
       "testfiles/default-view.txt"
       id),
    ("zero width skips wrapping", checkEditView
       "testfiles/original-long.txt"
       "testfiles/no-wrap-view.txt" $
       viewerResizeAction (0,snd defaultView)),
    ("zero height skips truncation", checkEditView
       "testfiles/original-long.txt"
       "testfiles/no-bound-view.txt" $
       composeActions [
           viewerResizeAction (fst defaultView,0),
           repeatAction 4 editorDownAction
         ]),
    ("move cursor below bottom", checkEditView
       "testfiles/original-long.txt"
       "testfiles/below-view.txt" $
       repeatAction 15 editorDownAction),
    ("scroll below end of doc", checkEditView
       "testfiles/original-long.txt"
       "testfiles/below-bottom-view.txt" $
       repeatAction 100 editorDownAction),
    ("page down puts edit at top and preserves cursor", checkEditView
       "testfiles/original-long.txt"
       "testfiles/page-down-view.txt" $
       composeActions [
           repeatAction 4 editorRightAction,
           editorPageDownAction,
           editorAppendAction "XYZ"
         ]),
    ("page up puts edit at top and preserves cursor", checkEditView
       "testfiles/original-long.txt"
       "testfiles/page-up-view.txt" $
       composeActions [
           repeatAction 4 editorRightAction,
           repeatAction 15 editorDownAction,
           editorPageUpAction,
           editorAppendAction "XYZ"
         ]),
    ("move previous at doc front", checkEditView
       "testfiles/original-long.txt"
       "testfiles/insert-front-view.txt" $
       composeActions [
           editorLeftAction,
           editorAppendAction "XYZ"
         ]),
    ("move next at doc back", checkEditView
       "testfiles/original-long.txt"
       "testfiles/insert-back-view.txt" $
       composeActions [
           -- 43 total lines after wrapping => down 42 then to end of line.
           repeatAction 43 editorDownAction,
           editorRightAction,
           editorAppendAction "XYZ"
         ]),
    ("insert in middle view", checkEditView
       "testfiles/original-long.txt"
       "testfiles/insert-middle-view.txt" $
       composeActions [
           repeatAction 15 editorDownAction,
           repeatAction 5 editorUpAction,
           repeatAction 7 editorRightAction,
           editorAppendAction "XYZ"
         ]),
    ("line position preserved when passing short lines", checkEditView
       "testfiles/original-long.txt"
       "testfiles/insert-middle-view.txt" $
       composeActions [
           repeatAction 7 editorRightAction,
           -- This traverses past empty lines after the line position is set.
           repeatAction 15 editorDownAction,
           repeatAction 5 editorUpAction,
           editorAppendAction "XYZ"
         ]),
    ("insert in middle content", checkEditContent
       "testfiles/original-long.txt"
       "testfiles/insert-middle-flat.txt" $
       composeActions [
           repeatAction 15 editorDownAction,
           repeatAction 5 editorUpAction,
           repeatAction 7 editorRightAction,
           editorAppendAction "XYZ"
         ]),
    ("delete in middle view", checkEditView
       "testfiles/original-long.txt"
       "testfiles/delete-middle-view.txt" $
       composeActions [
           repeatAction 15 editorDownAction,
           repeatAction 5 editorUpAction,
           repeatAction 7 editorRightAction,
           repeatAction 3 editorBackspaceAction
         ]),
    ("delete in middle content", checkEditContent
       "testfiles/original-long.txt"
       "testfiles/delete-middle-flat.txt" $
       composeActions [
           repeatAction 15 editorDownAction,
           repeatAction 5 editorUpAction,
           repeatAction 7 editorRightAction,
           repeatAction 3 editorBackspaceAction
         ]),
    ("join with previous", checkEditView
       "testfiles/original-long.txt"
       "testfiles/join-prev-view.txt" $
       composeActions [
           repeatAction 9 editorDownAction,
           editorBackspaceAction
         ]),
    ("join with next", checkEditView
       "testfiles/original-long.txt"
       "testfiles/join-next-view.txt" $
       composeActions [
           repeatAction 9 editorDownAction,
           editorLeftAction,
           editorDeleteAction
         ]),
    ("break in middle before", checkEditView
       "testfiles/original-long.txt"
       "testfiles/break-before-view.txt" $
       composeActions [
           repeatAction 15 editorDownAction,
           repeatAction 5 editorUpAction,
           repeatAction 7 editorRightAction,
           editorEnterAction,
           editorAppendAction "XYZ"
         ]),
    ("break in middle after", checkEditView
       "testfiles/original-long.txt"
       "testfiles/break-after-view.txt" $
       composeActions [
           repeatAction 15 editorDownAction,
           repeatAction 5 editorUpAction,
           repeatAction 7 editorRightAction,
           flip breakPara EditAfter,
           editorInsertAction "XYZ"
         ]),
    ("resize smaller preserves line offset and cursor", checkEditView
       "testfiles/original-long.txt"
       "testfiles/resize-smaller-view.txt" $
       composeActions [
           repeatAction 15 editorDownAction,
           repeatAction 5 editorUpAction,
           repeatAction 3 editorRightAction,
           viewerResizeAction smallerView,
           editorInsertAction "XYZ"
         ]),
    ("resize smaller truncates offset", checkEditView
       "testfiles/original-long.txt"
       "testfiles/resize-truncate-view.txt" $
       composeActions [
           repeatAction 9 editorDownAction,
           editorInsertAction "XYZ"
         ]),
    ("resize larger preserves line offset and cursor", checkEditView
       "testfiles/original-long.txt"
       "testfiles/resize-larger-view.txt" $
       composeActions [
           repeatAction 15 editorDownAction,
           repeatAction 5 editorUpAction,
           repeatAction 3 editorRightAction,
           viewerResizeAction largerView,
           editorInsertAction "XYZ"
         ]),
    ("cursor/edit defaults to top left", checkEditCursor breakExact
       "testfiles/original-long.txt" (0,0) (0,0) $
       id),
    ("cursor/edit in middle of view", checkEditCursor breakExact
       "testfiles/original-long.txt" (5,5) (2,25) $
       composeActions [
           -- NOTE: Keep horizontal first here.
           repeatAction 5 editorRightAction,
           repeatAction 5 editorDownAction
         ]),
    ("cursor/edit after insert before", checkEditCursor breakExact
       "testfiles/original-long.txt" (8,5) (2,28) $
       composeActions [
           repeatAction 5 editorRightAction,
           repeatAction 5 editorDownAction,
           editorAppendAction "XYZ"
         ]),
    ("cursor/edit after insert after", checkEditCursor breakExact
       "testfiles/original-long.txt" (5,5) (2,25) $
       composeActions [
           repeatAction 5 editorRightAction,
           repeatAction 5 editorDownAction,
           editorInsertAction "XYZ"
         ]),
    ("cursor/edit after delete before", checkEditCursor breakExact
       "testfiles/original-long.txt" (4,5) (2,24) $
       composeActions [
           repeatAction 5 editorRightAction,
           repeatAction 5 editorDownAction,
           editorBackspaceAction
         ]),
    ("cursor/edit after delete after", checkEditCursor breakExact
       "testfiles/original-long.txt" (5,5) (2,25) $
       composeActions [
           repeatAction 5 editorRightAction,
           repeatAction 5 editorDownAction,
           editorDeleteAction
         ]),
    ("cursor/edit after insert before line front", checkEditCursor breakExact
       "testfiles/original-long.txt" (3,5) (2,23) $
       composeActions [
           repeatAction 5 editorDownAction,
           editorAppendAction "XYZ"
         ]),
    ("cursor/edit after insert after line back", checkEditCursor breakExact
       "testfiles/original-long.txt" (20,5) (2,40) $
       composeActions [
           repeatAction 5 editorDownAction,
           editorEndAction,
           editorInsertAction "XYZ"
         ]),
    ("cursor/edit after delete before line front", checkEditCursor breakExact
       "testfiles/original-long.txt" (19,4) (2,19) $
       composeActions [
           repeatAction 5 editorDownAction,
           editorBackspaceAction
         ]),
    ("cursor/edit after delete after line back", checkEditCursor breakExact
       "testfiles/original-long.txt" (20,5) (2,40) $
       composeActions [
           repeatAction 5 editorDownAction,
           editorEndAction,
           editorDeleteAction
         ]),
    ("cursor/edit scroll to previous line", checkEditCursor breakExact
       "testfiles/original-long.txt" (20,4) (2,20) $
       composeActions [
           repeatAction 5 editorDownAction,
           editorLeftAction
         ]),
    ("cursor/edit scroll to next line", checkEditCursor breakExact
       "testfiles/original-long.txt" (0,6) (2,40) $
       composeActions [
           repeatAction 5 editorDownAction,
           editorEndAction,
           editorRightAction
         ]),
    ("cursor/edit after join with prev", checkEditCursor breakExact
       "testfiles/original-long.txt" (8,8) (2,88) $
       composeActions [
           repeatAction 9 editorDownAction,
           editorBackspaceAction
         ]),
    ("cursor/edit after join with next", checkEditCursor breakExact
       "testfiles/original-long.txt" (8,8) (2,88) $
       composeActions [
           repeatAction 8 editorDownAction,
           editorEndAction,
           editorDeleteAction
         ]),
    ("cursor/edit after resize preserves offset", checkEditCursor breakExact
       "testfiles/original-long.txt" (0,3) (1,0) $
       composeActions [
           repeatAction 3 editorDownAction,
           viewerResizeAction largerView
         ]),
    ("cursor/edit after resize larger truncates offset", checkEditCursor breakExact
       "testfiles/original-short.txt" (20,2) (1,20) $
       composeActions [
           repeatAction 3 editorDownAction,
           viewerResizeAction largerView
         ]),
    ("cursor/edit after resize unbounded maximizes offset", checkEditCursor breakExact
       "testfiles/original-long.txt" (0,15) (4,60) $
       composeActions [
           repeatAction 15 editorDownAction,
           viewerResizeAction (fst defaultView,0)
         ]),
    ("cursor/edit after resize smaller accounts for new break", checkEditCursor breakExact
       "testfiles/original-long.txt" (2,4) (2,20) $
       composeActions [
           repeatAction 4 editorDownAction,
           editorEndAction,
           viewerResizeAction smallerView
         ]),
    ("cursor/edit after resize larger accounts for new break", checkEditCursor breakExact
       "testfiles/original-long.txt" (16,5) (2,40) $
       composeActions [
           repeatAction 5 editorDownAction,
           editorEndAction,
           viewerResizeAction largerView
         ]),
    ("cursor/edit position uses parser tweaking", checkEditCursor (breakWords noHyphen)
       "testfiles/original-long.txt" (17,0) (0,18) $
       composeActions [
           -- The first line has 18 chars with a space.
           editorDownAction,
           editorLeftAction
         ]),
    ("edit position is truncated within paragraph", checkEditCursor breakExact
       "testfiles/original-long.txt" (2,0) (3,42) $
       editorSetPositionAction (3,60)),
    ("negative edit position within paragraph", checkEditCursor breakExact
       "testfiles/original-long.txt" (0,0) (0,0) $
       editorSetPositionAction (0,-5)),
    ("edit position is truncated within document", checkEditCursor breakExact
       "testfiles/original-long.txt" (10,0) (12,10) $
       editorSetPositionAction (30,10)),
    ("negative edit position within document", checkEditCursor breakExact
       "testfiles/original-long.txt" (0,0) (0,0) $
       editorSetPositionAction (-5,0)),
    ("edit position can traverse upward", checkEditCursor breakExact
       "testfiles/original-long.txt" (20,9) (3,40) $
       composeActions [
           repeatAction 40 editorDownAction,
           editorSetPositionAction (3,40)
         ])
  ]

defaultView = (20,10)
largerView = (24,12)
smallerView = (18,9)

splitParas = map UnparsedPara . lines

joinParas = unlines . map upText

loadDoc b = viewerResizeAction defaultView . editDocument b . splitParas

checkEditContent fx fy f = do
  edit <- fmap (f . loadDoc breakExact) $ readFile fx
  view <- readFile fy
  let restored = joinParas $ exportData edit
  checkCondition ("\n" ++ restored) (restored == view)

checkEditView fx fy f = do
  edit <- fmap (f . loadDoc breakExact) $ readFile fx
  view <- fmap (map trimSpace . lines) $ readFile fy
  let restored = map trimSpace $ getVisible edit
  checkCondition ("\n" ++ unlines restored) (restored == view)

checkEditCursor b fx c e f = do
  edit <- fmap (f . loadDoc b) $ readFile fx
  let cursor = getCursor edit
  let point  = getEditPoint edit
  checkConditions [
      ("Cursor: " ++ show cursor,cursor == c),
      ("Edit: "   ++ show point, point == e)
    ]

-- Just in case the text editor used to create the test file prunes whitespace
-- from the end of the line.
trimSpace = reverse . trim . reverse where
  trim (' ':cs) = trim cs
  trim cs       = cs
