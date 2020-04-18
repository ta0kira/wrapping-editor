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

import System.FilePath ((</>))

import WEditor.Document
import WEditor.LineWrap

import Common


allTests :: [(String,IO (Maybe String))]
allTests = [
    ("document preserved", checkEditContent
       "original-long.txt"
       "original-long.txt"
       id),
    ("default view at top", checkEditView
       "original-long.txt"
       "default-view.txt" (0,0)
       id),
    ("zero width skips wrapping", checkEditView
       "original-long.txt"
       "no-wrap-view.txt" (0,0) $
       viewerResizeAction (0,snd defaultView)),
    ("zero height skips truncation", checkEditView
       "original-long.txt"
       "no-bound-view.txt" (0,4) $
       composeActions [
           viewerResizeAction (fst defaultView,0),
           repeatAction 4 editorDownAction
         ]),
    ("move cursor below bottom", checkEditView
       "original-long.txt"
       "below-view.txt" (0,9) $
       repeatAction 15 editorDownAction),
    ("scroll below end of doc", checkEditView
       "original-long.txt"
       "below-bottom-view.txt" (10,0) $
       repeatAction 100 editorDownAction),
    ("page down puts edit at top and preserves cursor", checkEditView
       "original-long.txt"
       "page-down-view.txt" (7,0) $
       composeActions [
           repeatAction 4 editorRightAction,
           editorPageDownAction,
           editorAppendAction "XYZ"
         ]),
    ("page up puts edit at top and preserves cursor", checkEditView
       "original-long.txt"
       "page-up-view.txt" (7,0) $
       composeActions [
           repeatAction 4 editorRightAction,
           repeatAction 15 editorDownAction,
           editorPageUpAction,
           editorAppendAction "XYZ"
         ]),
    ("fill view ignores already full", checkEditView
       "original-long.txt"
       "fill-noop-view.txt" (0,4) $
       composeActions [
           editorPageDownAction,
           repeatAction 4 editorDownAction,
           viewerFillAction
         ]),
    ("fill view with short text", checkEditView
       "original-short.txt"
       "fill-short-view.txt" (2,5) $
       composeActions [
           editorPageDownAction,
           -- Manually shift the view up by one line.
           editorUpAction,
           editorDownAction,
           viewerFillAction
         ]),
    ("fill view starting past end", checkEditView
       "original-long.txt"
       "fill-end-view.txt" (10,7) $
       composeActions [
           repeatAction 5 editorPageDownAction,
           repeatAction 2 editorUpAction,
           viewerFillAction
         ]),
    ("shift view up within bounds", checkEditView
       "original-long.txt"
       "shift-up-bounded-view.txt" (0,8) $
       composeActions [
           editorPageDownAction,
           repeatAction 4 editorDownAction,
           viewerShiftUpAction 4
         ]),
    ("shift view down within bounds", checkEditView
       "original-long.txt"
       "shift-down-bounded-view.txt" (0,4) $
       composeActions [
           editorPageDownAction,
           repeatAction 8 editorDownAction,
           viewerShiftDownAction 4
         ]),
    ("shift view up past doc front", checkEditView
       "original-long.txt"
       "shift-up-unbounded-view.txt" (0,4) $
       composeActions [
           repeatAction 4 editorDownAction,
           viewerShiftUpAction 4
         ]),
    ("shift view down past doc back", checkEditView
       "original-long.txt"
       "shift-down-unbounded-view.txt" (10,0) $
       composeActions [
           repeatAction 5 editorPageDownAction,
           repeatAction 4 editorUpAction,
           viewerShiftDownAction 7
         ]),
    ("move previous at doc front", checkEditView
       "original-long.txt"
       "insert-front-view.txt" (3,0) $
       composeActions [
           editorLeftAction,
           editorAppendAction "XYZ"
         ]),
    ("move next at doc back", checkEditView
       "original-long.txt"
       "insert-back-view.txt" (13,9) $
       composeActions [
           -- 43 total lines after wrapping => down 42 then to end of line.
           repeatAction 43 editorDownAction,
           editorRightAction,
           editorAppendAction "XYZ"
         ]),
    ("insert in middle view", checkEditView
       "original-long.txt"
       "insert-middle-view.txt" (10,4) $
       composeActions [
           repeatAction 15 editorDownAction,
           repeatAction 5 editorUpAction,
           repeatAction 7 editorRightAction,
           editorAppendAction "XYZ"
         ]),
    ("line position preserved when passing short lines", checkEditView
       "original-long.txt"
       "insert-middle-view.txt" (10,4) $
       composeActions [
           repeatAction 7 editorRightAction,
           -- This traverses past empty lines after the line position is set.
           repeatAction 15 editorDownAction,
           repeatAction 5 editorUpAction,
           editorAppendAction "XYZ"
         ]),
    ("insert in middle content", checkEditContent
       "original-long.txt"
       "insert-middle-flat.txt" $
       composeActions [
           repeatAction 15 editorDownAction,
           repeatAction 5 editorUpAction,
           repeatAction 7 editorRightAction,
           editorAppendAction "XYZ"
         ]),
    ("delete in middle view", checkEditView
       "original-long.txt"
       "delete-middle-view.txt" (4,4) $
       composeActions [
           repeatAction 15 editorDownAction,
           repeatAction 5 editorUpAction,
           repeatAction 7 editorRightAction,
           repeatAction 3 editorBackspaceAction
         ]),
    ("delete in middle content", checkEditContent
       "original-long.txt"
       "delete-middle-flat.txt" $
       composeActions [
           repeatAction 15 editorDownAction,
           repeatAction 5 editorUpAction,
           repeatAction 7 editorRightAction,
           repeatAction 3 editorBackspaceAction
         ]),
    ("join with previous", checkEditView
       "original-long.txt"
       "join-prev-view.txt" (8,8) $
       composeActions [
           repeatAction 9 editorDownAction,
           editorBackspaceAction
         ]),
    ("join with next", checkEditView
       "original-long.txt"
       "join-next-view.txt" (8,8) $
       composeActions [
           repeatAction 9 editorDownAction,
           editorLeftAction,
           editorDeleteAction
         ]),
    ("break in middle before", checkEditView
       "original-long.txt"
       "break-before-view.txt" (3,5) $
       composeActions [
           repeatAction 15 editorDownAction,
           repeatAction 5 editorUpAction,
           repeatAction 7 editorRightAction,
           editorEnterAction,
           editorAppendAction "XYZ"
         ]),
    ("break in middle after", checkEditView
       "original-long.txt"
       "break-after-view.txt" (7,4) $
       composeActions [
           repeatAction 15 editorDownAction,
           repeatAction 5 editorUpAction,
           repeatAction 7 editorRightAction,
           flip breakPara EditAfter,
           editorInsertAction "XYZ"
         ]),
    ("resize smaller preserves line offset and cursor", checkEditView
       "original-long.txt"
       "resize-smaller-view.txt" (5,4) $
       composeActions [
           repeatAction 15 editorDownAction,
           repeatAction 5 editorUpAction,
           repeatAction 3 editorRightAction,
           viewerResizeAction smallerView,
           editorInsertAction "XYZ"
         ]),
    ("resize smaller truncates offset", checkEditView
       "original-long.txt"
       "resize-truncate-view.txt" (0,9) $
       composeActions [
           repeatAction 9 editorDownAction,
           editorInsertAction "XYZ"
         ]),
    ("resize larger preserves line offset and cursor", checkEditView
       "original-long.txt"
       "resize-larger-view.txt" (23,4) $
       composeActions [
           repeatAction 15 editorDownAction,
           repeatAction 5 editorUpAction,
           repeatAction 3 editorRightAction,
           viewerResizeAction largerView,
           editorInsertAction "XYZ"
         ]),
    ("cursor/edit defaults to top left", checkEditCursor breakExact
       "original-long.txt" (0,0) (0,0) $
       id),
    ("cursor/edit in middle of view", checkEditCursor breakExact
       "original-long.txt" (5,5) (2,25) $
       composeActions [
           -- NOTE: Keep horizontal first here.
           repeatAction 5 editorRightAction,
           repeatAction 5 editorDownAction
         ]),
    ("cursor/edit after insert before", checkEditCursor breakExact
       "original-long.txt" (8,5) (2,28) $
       composeActions [
           repeatAction 5 editorRightAction,
           repeatAction 5 editorDownAction,
           editorAppendAction "XYZ"
         ]),
    ("cursor/edit after insert after", checkEditCursor breakExact
       "original-long.txt" (5,5) (2,25) $
       composeActions [
           repeatAction 5 editorRightAction,
           repeatAction 5 editorDownAction,
           editorInsertAction "XYZ"
         ]),
    ("cursor/edit after delete before", checkEditCursor breakExact
       "original-long.txt" (4,5) (2,24) $
       composeActions [
           repeatAction 5 editorRightAction,
           repeatAction 5 editorDownAction,
           editorBackspaceAction
         ]),
    ("cursor/edit after delete after", checkEditCursor breakExact
       "original-long.txt" (5,5) (2,25) $
       composeActions [
           repeatAction 5 editorRightAction,
           repeatAction 5 editorDownAction,
           editorDeleteAction
         ]),
    ("cursor/edit after insert before line front", checkEditCursor breakExact
       "original-long.txt" (3,5) (2,23) $
       composeActions [
           repeatAction 5 editorDownAction,
           editorAppendAction "XYZ"
         ]),
    ("cursor/edit after insert after line back", checkEditCursor breakExact
       "original-long.txt" (20,5) (2,40) $
       composeActions [
           repeatAction 5 editorDownAction,
           editorEndAction,
           editorInsertAction "XYZ"
         ]),
    ("cursor/edit after delete before line front", checkEditCursor breakExact
       "original-long.txt" (19,4) (2,19) $
       composeActions [
           repeatAction 5 editorDownAction,
           editorBackspaceAction
         ]),
    ("cursor/edit after delete after line back", checkEditCursor breakExact
       "original-long.txt" (20,5) (2,40) $
       composeActions [
           repeatAction 5 editorDownAction,
           editorEndAction,
           editorDeleteAction
         ]),
    ("cursor/edit scroll to previous line", checkEditCursor breakExact
       "original-long.txt" (20,4) (2,20) $
       composeActions [
           repeatAction 5 editorDownAction,
           editorLeftAction
         ]),
    ("cursor/edit scroll to next line", checkEditCursor breakExact
       "original-long.txt" (0,6) (2,40) $
       composeActions [
           repeatAction 5 editorDownAction,
           editorEndAction,
           editorRightAction
         ]),
    ("cursor/edit after join with prev", checkEditCursor breakExact
       "original-long.txt" (8,8) (2,88) $
       composeActions [
           repeatAction 9 editorDownAction,
           editorBackspaceAction
         ]),
    ("cursor/edit after join with next", checkEditCursor breakExact
       "original-long.txt" (8,8) (2,88) $
       composeActions [
           repeatAction 8 editorDownAction,
           editorEndAction,
           editorDeleteAction
         ]),
    ("cursor/edit after resize preserves offset", checkEditCursor breakExact
       "original-long.txt" (0,3) (1,0) $
       composeActions [
           repeatAction 3 editorDownAction,
           viewerResizeAction largerView
         ]),
    ("cursor/edit after resize larger truncates offset", checkEditCursor breakExact
       "original-short.txt" (20,2) (1,20) $
       composeActions [
           repeatAction 3 editorDownAction,
           viewerResizeAction largerView
         ]),
    ("cursor/edit after resize unbounded maximizes offset", checkEditCursor breakExact
       "original-long.txt" (0,15) (4,60) $
       composeActions [
           repeatAction 15 editorDownAction,
           viewerResizeAction (fst defaultView,0)
         ]),
    ("cursor/edit after resize smaller accounts for new break", checkEditCursor breakExact
       "original-long.txt" (2,4) (2,20) $
       composeActions [
           repeatAction 4 editorDownAction,
           editorEndAction,
           viewerResizeAction smallerView
         ]),
    ("cursor/edit after resize larger accounts for new break", checkEditCursor breakExact
       "original-long.txt" (16,5) (2,40) $
       composeActions [
           repeatAction 5 editorDownAction,
           editorEndAction,
           viewerResizeAction largerView
         ]),
    ("cursor/edit position uses parser tweaking", checkEditCursor (breakWords noHyphen)
       "original-long.txt" (17,0) (0,18) $
       composeActions [
           -- The first line has 18 chars with a space.
           editorDownAction,
           editorLeftAction
         ]),
    ("edit position is truncated within paragraph", checkEditCursor breakExact
       "original-long.txt" (2,0) (3,42) $
       editorSetPositionAction (3,60)),
    ("negative edit position within paragraph", checkEditCursor breakExact
       "original-long.txt" (0,0) (0,0) $
       editorSetPositionAction (0,-5)),
    ("edit position is truncated within document", checkEditCursor breakExact
       "original-long.txt" (10,0) (12,10) $
       editorSetPositionAction (30,10)),
    ("negative edit position within document", checkEditCursor breakExact
       "original-long.txt" (0,0) (0,0) $
       editorSetPositionAction (-5,0)),
    ("edit position can traverse upward", checkEditCursor breakExact
       "original-long.txt" (20,9) (3,40) $
       composeActions [
           repeatAction 40 editorDownAction,
           editorSetPositionAction (3,40)
         ])
  ]

testFilesRoot = "test" </> "testfiles"

defaultView = (20,10)
largerView = (24,12)
smallerView = (18,9)

splitParas = map UnparsedPara . lines

joinParas = unlines . map upText

loadDoc b = viewerResizeAction defaultView . editDocument b . splitParas

checkEditContent fx fy f = do
  edit <- fmap (f . loadDoc breakExact) $ readFile (testFilesRoot </> fx)
  view <- readFile (testFilesRoot </> fy)
  let restored = joinParas $ exportData edit
  checkCondition ("\n" ++ restored) (restored == view)

checkEditView fx fy c f = do
  edit <- fmap (f . loadDoc breakExact) $ readFile (testFilesRoot </> fx)
  view <- fmap (map trimSpace . lines)  $ readFile (testFilesRoot </> fy)
  let restored = map trimSpace $ getVisible edit
  let cursor = getCursor edit
  checkConditions [
      ("Cursor: " ++ show cursor,cursor == c),
      ("View:\n" ++ unlines restored,restored == view)
    ]

checkEditCursor b fx c e f = do
  edit <- fmap (f . loadDoc b) $ readFile (testFilesRoot </> fx)
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
