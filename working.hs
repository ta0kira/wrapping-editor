{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

import Data.Maybe
import System.IO

import Brick
import Brick.AttrMap
import Brick.Main
import Brick.Types
import Brick.Widgets.Core
import Data.Monoid
import Graphics.Vty (defAttr)
import Graphics.Vty.Attributes
import Graphics.Vty.Input
import Lens.Micro

import Document
import LineWrap


-- Editor.

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

newWrappingEditor b n cs = WrappingEditor n (editDocument b cs)

dumpWrappingEditor (WrappingEditor _ editor) = exportDocument editor

-- Testing.

handleEventsWith _ x (VtyEvent (EvKey KEsc [])) = halt x
handleEventsWith handler x (VtyEvent e) = continue =<< handler x e

app :: (FixedFontViewer a Char, FixedFontEditor a Char) => App (WrappingEditor a String) e String
app = App {
  appDraw = \edit -> [renderWrappingEditor True edit],
  appChooseCursor = const listToMaybe,
  appHandleEvent = handleEventsWith handleWrappingEditor,
  appStartEvent = return,
  appAttrMap = const (attrMap defAttr [])
}

fakeEditFile f = do
  contents <- fmap (map UnparsedPara . lines) $ readFile f
  let editor = newWrappingEditor breakExact "editor" contents
  modified <- defaultMain app editor >>= return . dumpWrappingEditor
  hPutStr stdout $ unlines $ map upText modified
