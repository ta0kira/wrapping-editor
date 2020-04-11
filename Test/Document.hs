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
       "Test/testfiles/testdata.txt"
       "Test/testfiles/testdata.txt"
       id),
    ("default view at top", checkEditView
       "Test/testfiles/testdata.txt"
       "Test/testfiles/default-view.txt"
       id),
    ("move cursor below bottom", checkEditView
       "Test/testfiles/testdata.txt"
       "Test/testfiles/below-view.txt" $
       repeatAction 15 editorDownAction),
    ("scroll below end of doc", checkEditView
       "Test/testfiles/testdata.txt"
       "Test/testfiles/below-bottom-view.txt" $
       repeatAction 100 editorDownAction),
    ("page down puts edit at top and preserves cursor", checkEditView
       "Test/testfiles/testdata.txt"
       "Test/testfiles/page-down-view.txt" $
       composeActions [
           repeatAction 4 editorRightAction,
           editorPageDownAction,
           editorAppendAction "XYZ"
         ]),
    ("page up puts edit at top and preserves cursor", checkEditView
       "Test/testfiles/testdata.txt"
       "Test/testfiles/page-up-view.txt" $
       composeActions [
           repeatAction 4 editorRightAction,
           repeatAction 15 editorDownAction,
           editorPageUpAction,
           editorAppendAction "XYZ"
         ]),
    ("move previous at doc front", checkEditView
       "Test/testfiles/testdata.txt"
       "Test/testfiles/insert-front-view.txt" $
       composeActions [
           editorLeftAction,
           editorAppendAction "XYZ"
         ]),
    ("move next at doc back", checkEditView
       "Test/testfiles/testdata.txt"
       "Test/testfiles/insert-back-view.txt" $
       composeActions [
           -- 43 total lines after wrapping => down 42 then to end of line.
           repeatAction 43 editorDownAction,
           editorRightAction,
           editorAppendAction "XYZ"
         ]),
    ("insert in middle view", checkEditView
       "Test/testfiles/testdata.txt"
       "Test/testfiles/insert-middle-view.txt" $
       composeActions [
           repeatAction 15 editorDownAction,
           repeatAction 5 editorUpAction,
           repeatAction 7 editorRightAction,
           editorAppendAction "XYZ"
         ]),
    ("line position preserved when passing short lines", checkEditView
       "Test/testfiles/testdata.txt"
       "Test/testfiles/insert-middle-view.txt" $
       composeActions [
           repeatAction 7 editorRightAction,
           -- This traverses past empty lines after the line position is set.
           repeatAction 15 editorDownAction,
           repeatAction 5 editorUpAction,
           editorAppendAction "XYZ"
         ]),
    ("insert in middle content", checkEditContent
       "Test/testfiles/testdata.txt"
       "Test/testfiles/insert-middle-flat.txt" $
       composeActions [
           repeatAction 15 editorDownAction,
           repeatAction 5 editorUpAction,
           repeatAction 7 editorRightAction,
           editorAppendAction "XYZ"
         ]),
    ("delete in middle view", checkEditView
       "Test/testfiles/testdata.txt"
       "Test/testfiles/delete-middle-view.txt" $
       composeActions [
           repeatAction 15 editorDownAction,
           repeatAction 5 editorUpAction,
           repeatAction 7 editorRightAction,
           repeatAction 3 editorBackspaceAction
         ]),
    ("delete in middle content", checkEditContent
       "Test/testfiles/testdata.txt"
       "Test/testfiles/delete-middle-flat.txt" $
       composeActions [
           repeatAction 15 editorDownAction,
           repeatAction 5 editorUpAction,
           repeatAction 7 editorRightAction,
           repeatAction 3 editorBackspaceAction
         ]),
    ("join with previous", checkEditView
       "Test/testfiles/testdata.txt"
       "Test/testfiles/join-prev-view.txt" $
       composeActions [
           repeatAction 9 editorDownAction,
           editorBackspaceAction
         ]),
    ("join with next", checkEditView
       "Test/testfiles/testdata.txt"
       "Test/testfiles/join-next-view.txt" $
       composeActions [
           repeatAction 9 editorDownAction,
           editorLeftAction,
           editorDeleteAction
         ]),
    ("break in middle before", checkEditView
       "Test/testfiles/testdata.txt"
       "Test/testfiles/break-before-view.txt" $
       composeActions [
           repeatAction 15 editorDownAction,
           repeatAction 5 editorUpAction,
           repeatAction 7 editorRightAction,
           editorEnterAction,
           editorAppendAction "XYZ"
         ]),
    ("break in middle after", checkEditView
       "Test/testfiles/testdata.txt"
       "Test/testfiles/break-after-view.txt" $
       composeActions [
           repeatAction 15 editorDownAction,
           repeatAction 5 editorUpAction,
           repeatAction 7 editorRightAction,
           flip breakPara EditAfter,
           editorInsertAction "XYZ"
         ]),
    ("resize smaller preserves line offset and cursor", checkEditView
       "Test/testfiles/testdata.txt"
       "Test/testfiles/resize-smaller-view.txt" $
       composeActions [
           repeatAction 15 editorDownAction,
           repeatAction 5 editorUpAction,
           repeatAction 3 editorRightAction,
           viewerResizeAction (18,9),
           editorInsertAction "XYZ"
         ]),
    ("resize larger preserves line offset and cursor", checkEditView
       "Test/testfiles/testdata.txt"
       "Test/testfiles/resize-larger-view.txt" $
       composeActions [
           repeatAction 15 editorDownAction,
           repeatAction 5 editorUpAction,
           repeatAction 3 editorRightAction,
           viewerResizeAction (24,12),
           editorInsertAction "XYZ"
         ])
  ]

defaultView = (20,10)

splitParas = map UnparsedPara . lines

joinParas = unlines . map upText

loadDoc = viewerResizeAction defaultView . editDocument breakExact . splitParas

checkEditContent fx fy f = do
  edit <- fmap (f . loadDoc) $ readFile fx
  view <- readFile fy
  let restored = joinParas $ exportDocument edit
  checkCondition (restored == view) ("\n" ++ restored)

checkEditView fx fy f = do
  edit <- fmap (f . loadDoc) $ readFile fx
  view <- fmap (map trimSpace . lines) $ readFile fy
  let restored = map trimSpace $ getVisible edit
  checkCondition (restored == view) ("\n" ++ unlines restored)

-- Just in case the text editor used to create the test file prunes whitespace
-- from the end of the line.
trimSpace = reverse . trim . reverse where
  trim (' ':cs) = trim cs
  trim cs       = cs
