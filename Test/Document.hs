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

module Test.Document (
  allTests,
) where

import LineWrap
import Document
import Test.Common


allTests :: [(String,IO (Maybe String))]
allTests = [
    ("document preserved", checkEditContent
       "Test/testfiles/original-long.txt"
       "Test/testfiles/original-long.txt"
       id),
    ("default view at top", checkEditView
       "Test/testfiles/original-long.txt"
       "Test/testfiles/default-view.txt"
       id),
    ("zero width skips wrapping", checkEditView
       "Test/testfiles/original-long.txt"
       "Test/testfiles/no-wrap-view.txt" $
       viewerResizeAction (0,snd defaultView)),
    ("zero height skips truncation", checkEditView
       "Test/testfiles/original-long.txt"
       "Test/testfiles/no-bound-view.txt" $
       composeActions [
           viewerResizeAction (fst defaultView,0),
           repeatAction 4 editorDownAction
         ]),
    ("move cursor below bottom", checkEditView
       "Test/testfiles/original-long.txt"
       "Test/testfiles/below-view.txt" $
       repeatAction 15 editorDownAction),
    ("scroll below end of doc", checkEditView
       "Test/testfiles/original-long.txt"
       "Test/testfiles/below-bottom-view.txt" $
       repeatAction 100 editorDownAction),
    ("page down puts edit at top and preserves cursor", checkEditView
       "Test/testfiles/original-long.txt"
       "Test/testfiles/page-down-view.txt" $
       composeActions [
           repeatAction 4 editorRightAction,
           editorPageDownAction,
           editorAppendAction "XYZ"
         ]),
    ("page up puts edit at top and preserves cursor", checkEditView
       "Test/testfiles/original-long.txt"
       "Test/testfiles/page-up-view.txt" $
       composeActions [
           repeatAction 4 editorRightAction,
           repeatAction 15 editorDownAction,
           editorPageUpAction,
           editorAppendAction "XYZ"
         ]),
    ("move previous at doc front", checkEditView
       "Test/testfiles/original-long.txt"
       "Test/testfiles/insert-front-view.txt" $
       composeActions [
           editorLeftAction,
           editorAppendAction "XYZ"
         ]),
    ("move next at doc back", checkEditView
       "Test/testfiles/original-long.txt"
       "Test/testfiles/insert-back-view.txt" $
       composeActions [
           -- 43 total lines after wrapping => down 42 then to end of line.
           repeatAction 43 editorDownAction,
           editorRightAction,
           editorAppendAction "XYZ"
         ]),
    ("insert in middle view", checkEditView
       "Test/testfiles/original-long.txt"
       "Test/testfiles/insert-middle-view.txt" $
       composeActions [
           repeatAction 15 editorDownAction,
           repeatAction 5 editorUpAction,
           repeatAction 7 editorRightAction,
           editorAppendAction "XYZ"
         ]),
    ("line position preserved when passing short lines", checkEditView
       "Test/testfiles/original-long.txt"
       "Test/testfiles/insert-middle-view.txt" $
       composeActions [
           repeatAction 7 editorRightAction,
           -- This traverses past empty lines after the line position is set.
           repeatAction 15 editorDownAction,
           repeatAction 5 editorUpAction,
           editorAppendAction "XYZ"
         ]),
    ("insert in middle content", checkEditContent
       "Test/testfiles/original-long.txt"
       "Test/testfiles/insert-middle-flat.txt" $
       composeActions [
           repeatAction 15 editorDownAction,
           repeatAction 5 editorUpAction,
           repeatAction 7 editorRightAction,
           editorAppendAction "XYZ"
         ]),
    ("delete in middle view", checkEditView
       "Test/testfiles/original-long.txt"
       "Test/testfiles/delete-middle-view.txt" $
       composeActions [
           repeatAction 15 editorDownAction,
           repeatAction 5 editorUpAction,
           repeatAction 7 editorRightAction,
           repeatAction 3 editorBackspaceAction
         ]),
    ("delete in middle content", checkEditContent
       "Test/testfiles/original-long.txt"
       "Test/testfiles/delete-middle-flat.txt" $
       composeActions [
           repeatAction 15 editorDownAction,
           repeatAction 5 editorUpAction,
           repeatAction 7 editorRightAction,
           repeatAction 3 editorBackspaceAction
         ]),
    ("join with previous", checkEditView
       "Test/testfiles/original-long.txt"
       "Test/testfiles/join-prev-view.txt" $
       composeActions [
           repeatAction 9 editorDownAction,
           editorBackspaceAction
         ]),
    ("join with next", checkEditView
       "Test/testfiles/original-long.txt"
       "Test/testfiles/join-next-view.txt" $
       composeActions [
           repeatAction 9 editorDownAction,
           editorLeftAction,
           editorDeleteAction
         ]),
    ("break in middle before", checkEditView
       "Test/testfiles/original-long.txt"
       "Test/testfiles/break-before-view.txt" $
       composeActions [
           repeatAction 15 editorDownAction,
           repeatAction 5 editorUpAction,
           repeatAction 7 editorRightAction,
           editorEnterAction,
           editorAppendAction "XYZ"
         ]),
    ("break in middle after", checkEditView
       "Test/testfiles/original-long.txt"
       "Test/testfiles/break-after-view.txt" $
       composeActions [
           repeatAction 15 editorDownAction,
           repeatAction 5 editorUpAction,
           repeatAction 7 editorRightAction,
           flip breakPara EditAfter,
           editorInsertAction "XYZ"
         ]),
    ("resize smaller preserves line offset and cursor", checkEditView
       "Test/testfiles/original-long.txt"
       "Test/testfiles/resize-smaller-view.txt" $
       composeActions [
           repeatAction 15 editorDownAction,
           repeatAction 5 editorUpAction,
           repeatAction 3 editorRightAction,
           viewerResizeAction smallerView,
           editorInsertAction "XYZ"
         ]),
    ("resize smaller truncates offset", checkEditView
       "Test/testfiles/original-long.txt"
       "Test/testfiles/resize-truncate-view.txt" $
       composeActions [
           repeatAction 9 editorDownAction,
           editorInsertAction "XYZ"
         ]),
    ("resize larger preserves line offset and cursor", checkEditView
       "Test/testfiles/original-long.txt"
       "Test/testfiles/resize-larger-view.txt" $
       composeActions [
           repeatAction 15 editorDownAction,
           repeatAction 5 editorUpAction,
           repeatAction 3 editorRightAction,
           viewerResizeAction largerView,
           editorInsertAction "XYZ"
         ]),
    ("cursor defaults to top left", checkEditCursor breakExact
       "Test/testfiles/original-long.txt" (0,0) $
       id),
    ("cursor in middle of view", checkEditCursor breakExact
       "Test/testfiles/original-long.txt" (5,5) $
       composeActions [
           -- NOTE: Keep horizontal first here.
           repeatAction 5 editorRightAction,
           repeatAction 5 editorDownAction
         ]),
    ("cursor after insert before", checkEditCursor breakExact
       "Test/testfiles/original-long.txt" (8,5) $
       composeActions [
           repeatAction 5 editorRightAction,
           repeatAction 5 editorDownAction,
           editorAppendAction "XYZ"
         ]),
    ("cursor after insert after", checkEditCursor breakExact
       "Test/testfiles/original-long.txt" (5,5) $
       composeActions [
           repeatAction 5 editorRightAction,
           repeatAction 5 editorDownAction,
           editorInsertAction "XYZ"
         ]),
    ("cursor after delete before", checkEditCursor breakExact
       "Test/testfiles/original-long.txt" (4,5) $
       composeActions [
           repeatAction 5 editorRightAction,
           repeatAction 5 editorDownAction,
           editorBackspaceAction
         ]),
    ("cursor after delete after", checkEditCursor breakExact
       "Test/testfiles/original-long.txt" (5,5) $
       composeActions [
           repeatAction 5 editorRightAction,
           repeatAction 5 editorDownAction,
           editorDeleteAction
         ]),
    ("cursor after insert before line front", checkEditCursor breakExact
       "Test/testfiles/original-long.txt" (3,5) $
       composeActions [
           repeatAction 5 editorDownAction,
           editorAppendAction "XYZ"
         ]),
    ("cursor after insert after line back", checkEditCursor breakExact
       "Test/testfiles/original-long.txt" (20,5) $
       composeActions [
           repeatAction 5 editorDownAction,
           editorEndAction,
           editorInsertAction "XYZ"
         ]),
    ("cursor after delete before line front", checkEditCursor breakExact
       "Test/testfiles/original-long.txt" (19,4) $
       composeActions [
           repeatAction 5 editorDownAction,
           editorBackspaceAction
         ]),
    ("cursor after delete after line back", checkEditCursor breakExact
       "Test/testfiles/original-long.txt" (20,5) $
       composeActions [
           repeatAction 5 editorDownAction,
           editorEndAction,
           editorDeleteAction
         ]),
    ("cursor scroll to previous line", checkEditCursor breakExact
       "Test/testfiles/original-long.txt" (20,4) $
       composeActions [
           repeatAction 5 editorDownAction,
           editorLeftAction
         ]),
    ("cursor scroll to previous line", checkEditCursor breakExact
       "Test/testfiles/original-long.txt" (0,6) $
       composeActions [
           repeatAction 5 editorDownAction,
           editorEndAction,
           editorRightAction
         ]),
    ("cursor after join with prev", checkEditCursor breakExact
       "Test/testfiles/original-long.txt" (8,8) $
       composeActions [
           repeatAction 9 editorDownAction,
           editorBackspaceAction
         ]),
    ("cursor after join with next", checkEditCursor breakExact
       "Test/testfiles/original-long.txt" (8,8) $
       composeActions [
           repeatAction 8 editorDownAction,
           editorEndAction,
           editorDeleteAction
         ]),
    ("cursor after resize preserves offset", checkEditCursor breakExact
       "Test/testfiles/original-long.txt" (0,3) $
       composeActions [
           repeatAction 3 editorDownAction,
           viewerResizeAction largerView
         ]),
    ("cursor after resize larger truncates offset", checkEditCursor breakExact
       "Test/testfiles/original-short.txt" (20,2) $
       composeActions [
           repeatAction 3 editorDownAction,
           viewerResizeAction largerView
         ]),
    ("cursor after resize unbounded maximizes offset", checkEditCursor breakExact
       "Test/testfiles/original-long.txt" (0,15) $
       composeActions [
           repeatAction 15 editorDownAction,
           viewerResizeAction (fst defaultView,0)
         ]),
    ("cursor after resize smaller accounts for new break", checkEditCursor breakExact
       "Test/testfiles/original-long.txt" (2,4) $
       composeActions [
           repeatAction 4 editorDownAction,
           editorEndAction,
           viewerResizeAction smallerView
         ]),
    ("cursor after resize larger accounts for new break", checkEditCursor breakExact
       "Test/testfiles/original-long.txt" (16,5) $
       composeActions [
           repeatAction 5 editorDownAction,
           editorEndAction,
           viewerResizeAction largerView
         ]),
    ("cursor position uses parser tweaking", checkEditCursor (breakWords noHyphen)
       "Test/testfiles/original-long.txt" (20,5) $
       composeActions [
           repeatAction 5 editorDownAction,
           editorEndAction,
           editorLeftAction
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
  let restored = joinParas $ exportDocument edit
  checkCondition ("\n" ++ restored) (restored == view)

checkEditView fx fy f = do
  edit <- fmap (f . loadDoc breakExact) $ readFile fx
  view <- fmap (map trimSpace . lines) $ readFile fy
  let restored = map trimSpace $ getVisible edit
  checkCondition ("\n" ++ unlines restored) (restored == view)

checkEditCursor b fx c f = do
  edit <- fmap (f . loadDoc b) $ readFile fx
  let cursor = getCursor edit
  checkCondition (show cursor) (cursor == c)

-- Just in case the text editor used to create the test file prunes whitespace
-- from the end of the line.
trimSpace = reverse . trim . reverse where
  trim (' ':cs) = trim cs
  trim cs       = cs
