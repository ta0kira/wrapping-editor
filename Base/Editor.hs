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

-- | Generic text-editing functionality.

{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Safe #-}

module Base.Editor (
  EditorAction,
  FixedFontEditor(..),
  editorAppendAction,
  editorBackspaceAction,
  editorDeleteAction,
  editorDownAction,
  editorEndAction,
  editorEnterAction,
  editorHomeAction,
  editorInsertAction,
  editorLeftAction,
  editorPageDownAction,
  editorPageUpAction,
  editorRightAction,
  editorUpAction,
) where

import Base.Actions
import Base.Para


-- | Generic text editor for fixed-width fonts.
class FixedFontEditor a c | a -> c where
  -- | Apply an EditAction.
  editText :: a -> EditAction c -> EditDirection -> a
  -- | Break the current paragraph at the cursor.
  breakPara :: a -> EditDirection -> a
  -- | Apply a cursor movement.
  moveCursor :: a -> MoveDirection -> a
  -- | Get the cursor position, relative to the viewport if applicable.
  getCursor :: a -> (Int,Int)
  -- | Export the modified data.
  exportData :: a -> [UnparsedPara c]

-- | Any action that updates a 'FixedFontEditor'.
type EditorAction c = forall a. FixedFontEditor a c => a -> a

-- | Action for the Backspace key.
editorBackspaceAction :: EditorAction c
editorBackspaceAction e = editText e DeleteText EditBefore

-- | Action for the Delete key.
editorDeleteAction :: EditorAction c
editorDeleteAction e = editText e DeleteText EditAfter

-- | Action for the Enter key.
editorEnterAction :: EditorAction c
editorEnterAction e = breakPara e EditBefore

-- | Action for normal character insertion.
editorAppendAction :: [c] -> EditorAction c
editorAppendAction cs e = editText e (InsertText cs) EditBefore

-- | Action to insert after the cursor.
editorInsertAction :: [c] -> EditorAction c
editorInsertAction cs e = editText e (InsertText cs) EditAfter

-- | Action for the up-arrow key.
editorUpAction :: EditorAction c
editorUpAction e = moveCursor e MoveUp

-- | Action for the down-arrow key.
editorDownAction :: EditorAction c
editorDownAction e = moveCursor e MoveDown

-- | Action for the left-arrow key.
editorLeftAction :: EditorAction c
editorLeftAction e = moveCursor e MovePrev

-- | Action for the right-arrow key.
editorRightAction :: EditorAction c
editorRightAction e = moveCursor e MoveNext

-- | Action for the Home key.
editorHomeAction :: EditorAction c
editorHomeAction e = moveCursor e MoveHome

-- | Action for the End key.
editorEndAction :: EditorAction c
editorEndAction e = moveCursor e MoveEnd

-- | Action for the PageUp key.
editorPageUpAction :: EditorAction c
editorPageUpAction e = moveCursor e MovePageUp

-- | Action for the PageDown key.
editorPageDownAction :: EditorAction c
editorPageDownAction e = moveCursor e MovePageDown
