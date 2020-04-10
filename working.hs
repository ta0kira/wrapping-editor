{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

import Data.Maybe
import System.IO

import Brick.AttrMap
import Brick.Main
import Brick.Types
import Graphics.Vty (defAttr)
import Graphics.Vty.Input

import LineWrap
import WrappingEditor


handleEventsWith _ x (VtyEvent (EvKey KEsc [])) = halt x
handleEventsWith handler x (VtyEvent e) = continue =<< handler x e

app = App {
  appDraw = \edit -> [renderWrappingEditor True edit],
  appChooseCursor = const listToMaybe,
  appHandleEvent = handleEventsWith handleWrappingEditor,
  appStartEvent = return,
  appAttrMap = const (attrMap defAttr [])
}

fakeEditFile f = do
  contents <- fmap lines $ readFile f
  let editor = guessWrappingEditorSize (0,50) $ newWrappingEditor breakExact "editor" contents
  modified <- defaultMain app editor >>= return . dumpWrappingEditor
  hPutStr stdout $ unlines modified
