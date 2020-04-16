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

-- | A wrapping text-editor with dynamic sizing for
--   <https://github.com/jtdaugherty/brick Brick>.

{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Trustworthy #-}

module WEditorBrick.WrappingEditor (
  WrappingEditor,
  WrappingEditorAction,
  WrappingEditorDoer,
  doWrappingEditor,
  dumpWrappingEditor,
  genericWrappingEditor,
  handleWrappingEditor,
  mapWrappingEditor,
  newWrappingEditor,
  renderWrappingEditor,
) where

import Brick.Main
import Brick.Types
import Brick.Widgets.Core
import Graphics.Vty.Input
import Lens.Micro
import WEditor.Base
import WEditor.Document


-- | Create a new 'WrappingEditor' using the default editor component.
newWrappingEditor :: FixedFontParser a c => a -> n -> [[c]] -> WrappingEditor c n
newWrappingEditor b n cs = genericWrappingEditor n $ editDocument b $ map UnparsedPara cs

-- | Create a new 'WrappingEditor' using a custom editor component.
genericWrappingEditor :: (FixedFontViewer a c, FixedFontEditor a c) => n -> a -> WrappingEditor c n
genericWrappingEditor = WrappingEditor

-- | Any action that updates the editor state.
type WrappingEditorAction c = forall a. (FixedFontViewer a c, FixedFontEditor a c) => a -> a

-- | Update the editor state.
mapWrappingEditor :: WrappingEditorAction c -> WrappingEditor c n -> WrappingEditor c n
mapWrappingEditor f (WrappingEditor name editor) = WrappingEditor name (f editor)

-- | Any action that reads the editor state.
type WrappingEditorDoer c b = forall a. (FixedFontViewer a c, FixedFontEditor a c) => a -> b

-- | Read from the editor state.
doWrappingEditor :: WrappingEditorDoer c b -> WrappingEditor c n -> b
doWrappingEditor f (WrappingEditor _ editor) = f editor

-- | Dump the final contents of the edited document.
dumpWrappingEditor :: WrappingEditor c n -> [[c]]
dumpWrappingEditor = map upText . doWrappingEditor exportData

-- | Render the editor as a 'Widget'.
renderWrappingEditor :: (Ord n, Show n) => Bool -> WrappingEditor Char n -> Widget n
renderWrappingEditor focus editor = doWrappingEditor edit editor where
  edit e = Widget Greedy Greedy $ do
    ctx <- getContext
    let width = ctx^.availWidthL
    let height = ctx^.availHeightL
    -- NOTE: Resizing is a no-op if the size is unchanged.
    let e' = if height > 0
                then viewerResizeAction (width,height) e
                else e
    render $ viewport (getName editor) Vertical $ setCursor e' $ textArea width height e' where
      setCursor
        | focus = showCursor (getName editor) . Location . getCursor
        | otherwise = const id
      textArea w h = vBox . lineFill w h . map (strFill w) . getVisible
      strFill w cs = str $ take w $ cs ++ repeat ' '
      lineFill w h ls = take h $ ls ++ repeat (strFill w "")

-- | Update the editor based on Brick events.
handleWrappingEditor :: (Eq n) => WrappingEditor Char n -> Event -> EventM n (WrappingEditor Char n)
handleWrappingEditor editor event = do
  extent <- lookupExtent (getName editor)
  return $ mapWrappingEditor (action . resizeAction extent) editor where
    action :: EditorAction Char
    action =
      case event of
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
    resizeAction (Just ext) | snd (extentSize ext) > 0 = viewerResizeAction (extentSize ext)
    resizeAction  _ = id

-- | Editor widget for use with Brick.
data WrappingEditor c n =
  forall a. (FixedFontViewer a c, FixedFontEditor a c) => WrappingEditor {
    weName :: n,
    weEditor :: a
  }

instance Show n => Show (WrappingEditor c n) where
  show (WrappingEditor name editor) =
    "WrappingEditor { name: " ++ show name ++
                   ", size: " ++ show (getViewSize editor) ++
                   ", cursor: " ++ show (getCursor editor) ++
                   ", point: " ++ show (getEditPoint editor) ++ " }"

instance Named (WrappingEditor c n) n where
    getName = weName
