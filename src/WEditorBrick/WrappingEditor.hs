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
  doEditor,
  dumpEditor,
  genericEditor,
  handleEditor,
  mapEditor,
  newEditor,
  renderEditor,
  updateEditorExtent,
) where

import Brick.Main
import Brick.Types
import Brick.Widgets.Core
import Graphics.Vty.Input
import Lens.Micro
import WEditor.Base
import WEditor.Document


-- | Create a new 'WrappingEditor' using the default editor component.
newEditor :: FixedFontParser p c => p -> n -> [[c]] -> WrappingEditor c n
newEditor b n cs = genericEditor n $ editDocument b $ map UnparsedPara cs

-- | Create a new 'WrappingEditor' using a custom editor component.
genericEditor :: (FixedFontViewer e c, FixedFontEditor e c) => n -> e -> WrappingEditor c n
genericEditor = WrappingEditor

-- | Any action that updates the editor state.
type WrappingEditorAction c = forall e. (FixedFontViewer e c, FixedFontEditor e c) => e -> e

-- | Update the editor state.
mapEditor :: WrappingEditorAction c -> WrappingEditor c n -> WrappingEditor c n
mapEditor f (WrappingEditor name editor) = WrappingEditor name (f editor)

-- | Any action that reads the editor state.
type WrappingEditorDoer c b = forall e. (FixedFontViewer e c, FixedFontEditor e c) => e -> b

-- | Read from the editor state.
doEditor :: WrappingEditorDoer c b -> WrappingEditor c n -> b
doEditor f (WrappingEditor _ editor) = f editor

-- | Dump the final contents of the edited document.
dumpEditor :: WrappingEditor c n -> [[c]]
dumpEditor = map upText . doEditor exportData

-- | Render the editor as a 'Widget'.
renderEditor :: (Ord n, Show n) => Bool -> WrappingEditor Char n -> Widget n
renderEditor focus editor = doEditor view editor where
  view e = Widget Greedy Greedy $ do
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

-- | Update the viewport size based on the most-recent rendering of the editor.
--
--   Call this before any custom event-handling logic so that the viewport is
--   the correct size. This will ensure that vertical cursor movements match
--   what the user expects.
updateEditorExtent :: Eq n => WrappingEditor c n -> (EventM n (WrappingEditor c n))
updateEditorExtent editor = do
  extent <- lookupExtent (getName editor)
  return $ mapEditor (resize extent) editor where
    resize (Just ext) | snd (extentSize ext) > 0 = viewerResizeAction (extentSize ext)
    resize  _ = id

-- | Update the editor based on Brick events.
--
--   In addition to the canonical typing events, this handler also supports:
--
--     * @PageUp@, @PageDown@, @Home@, and @End@ keys.
--     * @Alt@+@Up@ shifts the view upward one line.
--     * @Alt@+@Down@ shifts the view downward one line.
--     * @Alt@+@Home@ shifts the view to hide empty space at the bottom.
--
--   To disable or override any of these keys, intercept them in the main
--   handler for the 'App'.
handleEditor :: Eq n => WrappingEditor Char n -> Event -> EventM n (WrappingEditor Char n)
handleEditor editor event = do
  extent <- lookupExtent (getName editor)
  updateEditorExtent editor >>= return . mapEditor action where
    action :: WrappingEditorAction Char
    action =
      case event of
           EvKey KBS []        -> editorBackspaceAction
           EvKey KDel []       -> editorDeleteAction
           EvKey KDown []      -> editorDownAction
           EvKey KEnd []       -> editorEndAction
           EvKey KEnter []     -> editorEnterAction
           EvKey KHome []      -> editorHomeAction
           EvKey KLeft []      -> editorLeftAction
           EvKey KPageDown []  -> editorPageDownAction
           EvKey KPageUp []    -> editorPageUpAction
           EvKey KRight []     -> editorRightAction
           EvKey KUp []        -> editorUpAction
           EvKey KDown [MMeta] -> viewerShiftDownAction 1
           EvKey KUp [MMeta]   -> viewerShiftUpAction   1
           EvKey KHome [MMeta] -> viewerFillAction
           EvKey (KChar c) [] | not (c `elem` "\t\r\n") -> editorAppendAction [c]
           _ -> id

-- | Editor widget for use with Brick.
data WrappingEditor c n =
  forall e. (FixedFontViewer e c, FixedFontEditor e c) => WrappingEditor {
    weName :: n,
    weEditor :: e
  }

instance Show n => Show (WrappingEditor c n) where
  show (WrappingEditor name editor) =
    "WrappingEditor { name: " ++ show name ++
                   ", size: " ++ show (getViewSize editor) ++
                   ", cursor: " ++ show (getCursor editor) ++
                   ", point: " ++ show (getEditPoint editor) ++ " }"

instance Named (WrappingEditor c n) n where
    getName = weName
