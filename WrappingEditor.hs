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
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Trustworthy #-}

module WrappingEditor (
  WrappingEditor,
  dumpWrappingEditor,
  guessWrappingEditorSize,
  handleWrappingEditor,
  newWrappingEditor,
  renderWrappingEditor,
) where

import Brick.Types
import Brick.Widgets.Core
import Graphics.Vty.Input
import Lens.Micro

import Document


newWrappingEditor :: FixedFontParser a c b => a -> n -> [[c]] -> WrappingEditor (EditingDocument c b) n
newWrappingEditor b n cs = WrappingEditor n $ editDocument b $ map UnparsedPara cs

dumpWrappingEditor :: WrappingEditor (EditingDocument c b) n -> [[c]]
dumpWrappingEditor (WrappingEditor _ editor) = map upText $ exportDocument editor

guessWrappingEditorSize :: (Int,Int) -> WrappingEditor (EditingDocument c b) n -> WrappingEditor (EditingDocument c b) n
guessWrappingEditorSize size (WrappingEditor n editor) =
  (WrappingEditor n (viewerResizeAction size editor))

renderWrappingEditor focus (WrappingEditor n editor) = Widget Greedy Greedy $ do
  ctx <- getContext
  let width = ctx^.availWidthL
  let height = ctx^.availHeightL
  let editor' = viewerResizeAction (width,height) editor
  render $ setCursor editor' $ textArea width height editor' where
    setCursor
      | focus = showCursor n . Location . getCursor
      | otherwise = const id
    textArea w h = vBox . lineFill w h . map (strFill w) . getVisible
    strFill w cs = str $ take w $ cs ++ repeat ' '
    lineFill w h ls = take h $ ls ++ repeat (strFill w "")

handleWrappingEditor (WrappingEditor n editor) event = return (WrappingEditor n (action editor)) where
  action = case event of
                EvKey KEnter [] -> editorEnterAction
                EvKey KDel [] ->  editorDeleteAction
                EvKey KBS [] -> editorBackspaceAction
                EvKey KUp [] -> editorUpAction
                EvKey KDown [] ->  editorDownAction
                EvKey KLeft [] -> editorLeftAction
                EvKey KRight [] -> editorRightAction
                EvKey (KChar c) [] | not (c `elem` "\t\r\n") -> editorAppendAction [c]
                _ -> id

data WrappingEditor e n =
  WrappingEditor {
    neName :: n,
    neEditor :: e
  }

instance Named (WrappingEditor e n) n where
    getName = neName
