{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Safe #-}

module Test.Document (
  allTests,
) where

import Document
import Test.Common


allTests :: [(String,IO (Maybe String))]
allTests = [
    ("default view at top", checkEditView
       "Test/testfiles/testdata.txt"
       "Test/testfiles/default-view.txt"
       id),
    ("move cursor below bottom", checkEditView
       "Test/testfiles/testdata.txt"
       "Test/testfiles/below-view.txt" $
       repeatAction 15 (flip moveCursor MoveDown)),
    ("insert in middle view", checkEditView
       "Test/testfiles/testdata.txt"
       "Test/testfiles/insert-middle-view.txt" $
       composeActions [
           repeatAction 15 (flip moveCursor MoveDown),
           repeatAction 5 (flip moveCursor MoveUp),
           repeatAction 7 (flip moveCursor MoveNext),
           flip2 editText (InsertText "XYZ") EditBefore
         ]),
    ("insert in middle content", checkEditContent
       "Test/testfiles/testdata.txt"
       "Test/testfiles/insert-middle-flat.txt" $
       composeActions [
           repeatAction 15 (flip moveCursor MoveDown),
           repeatAction 5 (flip moveCursor MoveUp),
           repeatAction 7 (flip moveCursor MoveNext),
           flip2 editText (InsertText "XYZ") EditBefore
         ]),
    ("delete in middle view", checkEditView
       "Test/testfiles/testdata.txt"
       "Test/testfiles/delete-middle-view.txt" $
       composeActions [
           repeatAction 15 (flip moveCursor MoveDown),
           repeatAction 5 (flip moveCursor MoveUp),
           repeatAction 7 (flip moveCursor MoveNext),
           repeatAction 3 (flip2 editText DeleteText EditBefore)
         ]),
    ("delete in middle content", checkEditContent
       "Test/testfiles/testdata.txt"
       "Test/testfiles/delete-middle-flat.txt" $
       composeActions [
           repeatAction 15 (flip moveCursor MoveDown),
           repeatAction 5 (flip moveCursor MoveUp),
           repeatAction 7 (flip moveCursor MoveNext),
           repeatAction 3 (flip2 editText DeleteText EditBefore)
         ]),
    ("join with previous", checkEditView
       "Test/testfiles/testdata.txt"
       "Test/testfiles/join-prev-view.txt" $
       composeActions [
           repeatAction 9 (flip moveCursor MoveDown),
           flip2 editText DeleteText EditBefore
         ]),
    ("join with next", checkEditView
       "Test/testfiles/testdata.txt"
       "Test/testfiles/join-next-view.txt" $
       composeActions [
           repeatAction 9 (flip moveCursor MoveDown),
           flip moveCursor MovePrev,
           flip2 editText DeleteText EditAfter
         ]),
    ("break in middle before", checkEditView
       "Test/testfiles/testdata.txt"
       "Test/testfiles/break-before-view.txt" $
       composeActions [
           repeatAction 15 (flip moveCursor MoveDown),
           repeatAction 5 (flip moveCursor MoveUp),
           repeatAction 7 (flip moveCursor MoveNext),
           flip breakPara EditBefore,
           flip2 editText (InsertText "XYZ") EditBefore
         ]),
    ("break in middle after", checkEditView
       "Test/testfiles/testdata.txt"
       "Test/testfiles/break-after-view.txt" $
       composeActions [
           repeatAction 15 (flip moveCursor MoveDown),
           repeatAction 5 (flip moveCursor MoveUp),
           repeatAction 7 (flip moveCursor MoveNext),
           flip breakPara EditAfter,
           flip2 editText (InsertText "XYZ") EditAfter
         ]),
    ("resize smaller preserves line offset and cursor", checkEditView
       "Test/testfiles/testdata.txt"
       "Test/testfiles/resize-smaller-view.txt" $
       composeActions [
           repeatAction 15 (flip moveCursor MoveDown),
           repeatAction 5 (flip moveCursor MoveUp),
           repeatAction 3 (flip moveCursor MoveNext),
           flip setViewSize (18,9),
           flip2 editText (InsertText "XYZ") EditAfter
         ]),
    ("resize larger preserves line offset and cursor", checkEditView
       "Test/testfiles/testdata.txt"
       "Test/testfiles/resize-larger-view.txt" $
       composeActions [
           repeatAction 15 (flip moveCursor MoveDown),
           repeatAction 5 (flip moveCursor MoveUp),
           repeatAction 3 (flip moveCursor MoveNext),
           flip setViewSize (24,12),
           flip2 editText (InsertText "XYZ") EditAfter
         ])
  ]

defaultView = (20,10)

flip2 f x y z = f z x y

checkEditContent fx fy f = do
  edit <- fmap (f . flip setViewSize defaultView . editDocument breakExact) $ readFile fx
  view <- readFile fy
  let restored = flattenDocument edit
  checkCondition (restored == view) ("\n" ++ restored)

checkEditView fx fy f = do
  edit <- fmap (f . flip setViewSize defaultView . editDocument breakExact) $ readFile fx
  view <- fmap (map trimSpace . lines) $ readFile fy
  let restored = map trimSpace $ getVisible edit
  checkCondition (restored == view) ("\n" ++ unlines restored)

-- Just in case the text editor used to create the test file prunes whitespace
-- from the end of the line.
trimSpace = reverse . trim . reverse where
  trim (' ':cs) = trim cs
  trim cs       = cs
