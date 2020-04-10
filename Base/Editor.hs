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
  editorEnterAction,
  editorInsertAction,
  editorLeftAction,
  editorRightAction,
  editorUpAction,
) where

import Base.Actions


class FixedFontEditor a c | a -> c where
  editText :: a -> EditAction c -> EditDirection -> a
  breakPara :: a -> EditDirection -> a
  moveCursor :: a -> MoveDirection -> a
  getCursor :: a -> (Int,Int)

type EditorAction c = forall a. FixedFontEditor a c => a -> a

editorBackspaceAction :: EditorAction c
editorBackspaceAction e = editText e DeleteText EditBefore

editorDeleteAction :: EditorAction c
editorDeleteAction e = editText e DeleteText EditAfter

editorEnterAction :: EditorAction c
editorEnterAction e = breakPara e EditBefore

editorAppendAction :: c -> EditorAction c
editorAppendAction c e = editText e (InsertText [c]) EditBefore

editorInsertAction :: c -> EditorAction c
editorInsertAction c e = editText e (InsertText [c]) EditAfter

editorUpAction :: EditorAction c
editorUpAction e = moveCursor e MoveUp

editorDownAction :: EditorAction c
editorDownAction e = moveCursor e MoveDown

editorLeftAction :: EditorAction c
editorLeftAction e = moveCursor e MovePrev

editorRightAction :: EditorAction c
editorRightAction e = moveCursor e MoveNext
