{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Safe #-}

import Control.Applicative
import Data.Traversable

class ListSplitter a b | a -> b where
  nextSplit :: a -> [b] -> ([b],[b])

splitList :: (Alternative t, ListSplitter a b) => a -> [b] -> t [b]
splitList f = splitAll where
  splitAll [] = empty
  splitAll xs = let (y,ys) = nextSplit f xs in (pure y)<|>(splitAll ys)

data FixedWidthSplitter =
  FixedWidthSplitter {
    fwsMax :: Int
  }

instance ListSplitter FixedWidthSplitter Char where
  nextSplit (FixedWidthSplitter n) xs = (take n xs,drop n xs)


data CursorMove = CursorUp | CursorDown | CursorPrev | CursorNext deriving (Show)

data EditAction c = InsertBefore c | InsertAfter c | DeleteBefore | DeleteAfter deriving (Show)

data ViewChange u = AbsoluteFrame u | RelativeFrame u | UpdateOffset u deriving (Show)

class WrappingEditor a c u | a -> c u where
  moveCursor :: a -> CursorMove -> a
  editData :: a -> EditAction c -> a
  changeView :: a -> ViewChange u -> a
  seekInView :: a -> u -> a
  getVisible :: a -> [[c]]
  getCursor :: a -> Maybe u


data LineEditor c =
  LineEditor {
    leCharsBefore :: [c],  -- Reversed.
    leCharsAfter :: [c]
  }
  deriving (Show)

data ParaEditor c =
  ParaEditor {
    peLinesBefore :: [[c]],  -- Reversed.
    peLineCurrent :: LineEditor c,
    peLinesAfter :: [[c]]
  } |
  EmptyParaEditor
  deriving (Show)

data ViewBuffer c =
  ViewBuffer {
    vbLinesBefore :: [[c]],  -- Reversed.
    vbLinesCurrent :: [[c]],
    vbLinesAfter :: [[c]]
  } |
  EmptyViewBuffer
  deriving (Show)

data FixedCharSize =
  FixedCharSize {
    fcsWidth :: Int,
    fcsHeight :: Int
  }

data TextEditor c =
  TextEditor {
    teViewSize :: FixedCharSize,
    teViewOffset :: FixedCharSize,
    teCursor :: Maybe FixedCharSize,
    teParaBefore :: [[c]],  -- Reversed.
    teParaCurrent :: ParaEditor c,
    teParaAfter :: [[c]],
    teRendered :: ViewBuffer c,
    teWrapFunction :: Int -> [c] -> ([c],[c])
  }

editDocument :: (Int -> [c] -> ([c],[c])) -> [[c]] -> FixedCharSize -> TextEditor c
editDocument wrap ps view = changeView editor (AbsoluteFrame view) where
  editor = TextEditor {
      teViewSize = FixedCharSize 0 0,
      teViewOffset = FixedCharSize 0 0,
      teCursor = Just (FixedCharSize 0 0),
      teParaBefore = [],
      teParaCurrent = EmptyParaEditor,
      teParaAfter = ps,
      teRendered = EmptyViewBuffer,
      teWrapFunction = wrap
    }

editParagraph :: ([c] -> ([c],[c])) -> [c] -> ParaEditor c
editParagraph wrap cs = editor where
  editor = ParaEditor {
      peLinesBefore = [],
      peLineCurrent = LineEditor {
          leCharsBefore  = [],
          leCharsAfter = first
        },
      peLinesAfter = rest
    }
  (first:rest) = nonempty (split cs)
  nonempty [] = [[]]
  nonempty ls = ls
  split [] = []
  split cs = let (l,cs2) = wrap cs in (split cs2) ++ [l]

instance WrappingEditor (TextEditor c) c FixedCharSize where
  moveCursor e _ =
    TextEditor {
      teViewSize = teViewSize e,
      teViewOffset = teViewOffset e,
      teCursor = teCursor e,
      teParaBefore = teParaBefore e,
      teParaCurrent = teParaCurrent e,
      teParaAfter = teParaAfter e,
      teRendered = teRendered e,
      teWrapFunction = teWrapFunction e
    }
  editData e _ =
    TextEditor {
      teViewSize = teViewSize e,
      teViewOffset = teViewOffset e,
      teCursor = teCursor e,
      teParaBefore = teParaBefore e,
      teParaCurrent = teParaCurrent e,
      teParaAfter = teParaAfter e,
      teRendered = teRendered e,
      teWrapFunction = teWrapFunction e
    }
  changeView e _ =
    TextEditor {
      teViewSize = teViewSize e,
      teViewOffset = teViewOffset e,
      teCursor = teCursor e,
      teParaBefore = teParaBefore e,
      teParaCurrent = teParaCurrent e,
      teParaAfter = teParaAfter e,
      teRendered = teRendered e,
      teWrapFunction = teWrapFunction e
    }
  seekInView e _ =
    TextEditor {
      teViewSize = teViewSize e,
      teViewOffset = teViewOffset e,
      teCursor = teCursor e,
      teParaBefore = teParaBefore e,
      teParaCurrent = teParaCurrent e,
      teParaAfter = teParaAfter e,
      teRendered = teRendered e,
      teWrapFunction = teWrapFunction e
    }
  getVisible = vbLinesCurrent . teRendered
  getCursor = teCursor
