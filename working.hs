{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Safe #-}


data CursorMove = CursorUp | CursorDown | CursorPrev | CursorNext deriving (Show)

data EditAction c = InsertBefore c | InsertAfter c | DeleteBefore | DeleteAfter deriving (Show)

data ViewChange u = AbsoluteFrame u | RelativeFrame u | UpdateOffset u deriving (Show)

class WrappingEditor a c | a -> c where
  moveCursor :: a -> CursorMove -> a
  editData :: a -> EditAction c -> a
  getAllContent :: a -> [c]

class WrappingEditor a c => ViewableEditor a c u | a -> c u where
  changeView :: a -> ViewChange u -> a
  seekInView :: a -> u -> a
  getVisible :: a -> [[c]]
  getCursor :: a -> Maybe u

splitAllLines :: ([c] -> (l,[c])) -> [c] -> [l]
splitAllLines f [] = []
splitAllLines f cs = let (l,cs2) = f cs in l:(splitAllLines f cs2)


data LineEditor c =
  LineEditor {
    leCharsBefore :: [c],  -- Reversed.
    leCharsAfter :: [c],
    lePosition :: Int,
    leSize :: Int
  }
  deriving (Show)

newLineEditor :: LineViewer c -> LineEditor c
newLineEditor (LineViewer cs) =
  LineEditor {
    leCharsBefore = [],
    leCharsAfter = cs,
    lePosition = 0,
    leSize = length cs
  }

getLineChar :: LineEditor c -> Int
getLineChar = lePosition

seekLineChar :: Int -> LineEditor c -> LineEditor c
seekLineChar n e@(LineEditor bs as p s)
  | p < n && not (null as) = seekLineChar n (LineEditor (head as:bs) (tail as) (p+1) s)
  | p > n && not (null bs) = seekLineChar n (LineEditor (tail bs) (head bs:as) (p-1) s)
  | otherwise = e

getFullLine :: LineEditor c -> LineViewer c
getFullLine e = LineViewer $ reverse (leCharsBefore e) ++ leCharsAfter e

getLineSize :: LineEditor c -> Int
getLineSize = leSize


-- NOTE: Using this instead of [c] allows for something like a line-break type
-- (e.g., hyphen, ellipsis) to be included later on.
data LineViewer c =
  LineViewer {
    lvLine :: [c]
  }
  deriving (Show)

newLineViewer :: [c] -> LineViewer c
newLineViewer cs = editor where
  editor = LineViewer {
      lvLine = cs
    }


data UnparsedPara c =
  UnparsedPara {
    upData :: [c]
  }
  deriving (Show)


data ParaEditor c =
  ParaEditor {
    peLinesBefore :: [LineViewer c],  -- Reversed.
    peLineCurrent :: LineEditor c,
    peLinesAfter :: [LineViewer c],
    pePosition :: Int
  }
  deriving (Show)

newParaEditor :: ([c] -> (LineViewer c,[c])) -> UnparsedPara c -> ParaEditor c
newParaEditor wrap (UnparsedPara cs) = editor where
  editor = ParaEditor {
      peLinesBefore = [],
      peLineCurrent = newLineEditor first,
      peLinesAfter = rest,
      pePosition = 0
    }
  (first:rest) = nonempty (splitAllLines wrap cs)
  nonempty [] = [LineViewer []]
  nonempty ls = ls

seekParaLine :: Int -> ParaEditor c -> ParaEditor c
seekParaLine n (ParaEditor bs l as p) = seek bs (getFullLine l) as p where
  seek bs l as p
    | p < n && not (null as) = seek (l:bs) (head as) (tail as) (p+1)
    | p > n && not (null bs) = seek (tail bs) (head bs) (l:as) (p-1)
    | otherwise = ParaEditor bs (newLineEditor l) as p


data ParaViewer c =
  ParaViewer {
    pvLines :: [LineViewer c]
  }
  deriving (Show)

newParaViewer :: ([c] -> (LineViewer c,[c])) -> UnparsedPara c -> ParaViewer c
newParaViewer wrap (UnparsedPara cs) = editor where
  editor = ParaViewer {
      pvLines = splitAllLines wrap cs
    }


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
  deriving (Show)


data TextEditor c =
  TextEditor {
    teHiddenBefore :: [UnparsedPara c],  -- Reversed.
    teVisibleBefore :: [ParaViewer c],  -- Reversed.
    teEditing :: ParaEditor c,
    teVisibleAfter :: [ParaViewer c],
    teHiddenAfter :: [UnparsedPara c]
  }
  deriving (Show)

newTextEditor :: ([c] -> (LineViewer c,[c])) -> [UnparsedPara c] -> TextEditor c
newTextEditor wrap [] = newTextEditor wrap [UnparsedPara []]
newTextEditor wrap (p:ps) =
  TextEditor {
    teHiddenBefore = [],
    teVisibleBefore = [],
    teEditing = newParaEditor wrap p,
    teVisibleAfter = [],
    teHiddenAfter = ps
  }


breakParagraphs = map UnparsedPara . lines

fixedLineBreaks n xs = (LineViewer $ take n xs,drop n xs)

main = do
  contents <- readFile "testdata.txt"
  let paras = breakParagraphs contents
  let editor = newTextEditor (fixedLineBreaks 5) paras
  putStrLn $ show editor
