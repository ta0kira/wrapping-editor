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
  handleWrappingEditor,
  newWrappingEditor,
  renderWrappingEditor,
) where

import Brick.Main
import Brick.Types
import Brick.Widgets.Core
import Graphics.Vty.Input
import Lens.Micro

import Document


newWrappingEditor :: FixedFontParser a c b => a -> n -> [[c]] -> WrappingEditor c n
newWrappingEditor b n cs = WrappingEditor n $ editDocument b $ map UnparsedPara cs

dumpWrappingEditor :: WrappingEditor c n -> [[c]]
dumpWrappingEditor (WrappingEditor _ editor) = map upText $ exportDocument editor

renderWrappingEditor :: (Ord n, Show n) => Bool -> WrappingEditor Char n -> Widget n
renderWrappingEditor focus (WrappingEditor n editor) = Widget Greedy Greedy $ do
  ctx <- getContext
  let width = ctx^.availWidthL
  let height = ctx^.availHeightL
  -- NOTE: Resizing is a no-op if the size is unchanged.
  let editor' = viewerResizeAction (width,height) editor
  render $ viewport n Vertical $ setCursor editor' $ textArea width height editor' where
    setCursor
      | focus = showCursor n . Location . getCursor
      | otherwise = const id
    textArea w h = vBox . lineFill w h . map (strFill w) . getVisible
    strFill w cs = str $ take w $ cs ++ repeat ' '
    lineFill w h ls = take h $ ls ++ repeat (strFill w "")

handleWrappingEditor :: (Eq n) => WrappingEditor Char n -> Event -> EventM n (WrappingEditor Char n)
handleWrappingEditor (WrappingEditor n editor) event = do
  let action = case event of
                    EvKey KBS []       -> editorBackspaceAction
                    EvKey KDel []      -> editorDeleteAction
                    EvKey KDown []     -> editorDownAction
                    EvKey KEnd []      -> editorEndAction
                    EvKey KEnter []    -> editorEnterAction
                    EvKey KHome []     -> editorHomeAction
                    EvKey KLeft []     -> editorLeftAction
                    EvKey KPageDown [] -> editorPageDownAction
                    EvKey KPageUp []   -> editorPageUpAction
                    EvKey KRight []    -> editorRightAction
                    EvKey KUp []       -> editorUpAction
                    EvKey (KChar c) [] | not (c `elem` "\t\r\n") -> editorAppendAction [c]
                    _ -> id
  editor' <- setSize editor >>= return . action
  return $ WrappingEditor n editor' where
    setSize editor = do
      extent <- lookupExtent n
      case extent of
           Nothing -> return editor
           (Just ext) -> return $ viewerResizeAction (extentSize ext) editor

data WrappingEditor c n =
  WrappingEditor {
    weName :: n,
    weEditor :: EditingDocument c
  }

instance Named (WrappingEditor c n) n where
    getName = weName
