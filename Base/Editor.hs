{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Safe #-}

module Base.Editor (
  EditorAction,
  FixedFontEditor(..),
  editorBackspaceAction,
  editorDeleteAction,
  editorDownAction,
  editorEnterAction,
  editorInsertAction,
  editorLeftAction,
  editorRightAction,
  editorTypeAction,
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

editorTypeAction :: c -> EditorAction c
editorTypeAction c e = editText e (InsertText [c]) EditBefore

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
