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

import Data.Maybe
import System.Environment
import System.Exit
import System.IO

import Brick.AttrMap
import Brick.Main
import Brick.Types
import Graphics.Vty (defAttr)
import Graphics.Vty.Input
import WEditor.LineWrap             -- For the line-wrapping policy.
import WEditorBrick.WrappingEditor  -- For the wrapping editor Brick widget.


-- Delegate most events to a single handler. If you get annoyed by scrolling
-- past the last line, you can apply mapEditor viewerFillAction before continue.
handleEventsWith _ x (VtyEvent (EvKey KEsc [])) = halt x
handleEventsWith handler x (VtyEvent e) = continue =<< handler x e

-- An app containing nothing but a single editor widget.
app = App {
  -- renderEditor renders the current editor in a viewport with the same name as
  -- the editor. True means that the editor has focus.
  appDraw = \edit -> [renderEditor True edit],
  appChooseCursor = const listToMaybe,
  -- handleEditor handles editor events such as cursor movements and typing.
  appHandleEvent = handleEventsWith handleEditor,
  appStartEvent = return,
  appAttrMap = const (attrMap defAttr [])
}

-- Loads the filename, runs the editor, and returns the final data.
-- NOTE: This *doesn't* modify the contents of the file.
fakeEditFile f = do
  contents <- fmap lines $ readFile f
  -- newEditor creates an editor object. breakWords is semi-aware of words, and
  -- lazyHyphen performs hyphenation.
  let editor = newEditor (breakWords lazyHyphen) "editor" contents
  -- dumpEditor extracts the editor's contents.
  modified <- defaultMain app editor >>= return . dumpEditor
  return $ unlines modified

main = do
  args <- getArgs
  case args of
       [f] -> do
         result <- fakeEditFile f
         hPutStr stdout result
         exitSuccess
       _ -> do
         hPutStrLn stderr "Pass a single filename, or call fakeEditFile from ghci."
         exitFailure
