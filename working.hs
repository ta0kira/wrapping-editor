{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Safe #-}

import Data.List (intercalate)


class RenderedLines a l | a -> l where
  getAllLines :: a -> [l]


data CursorMove = CursorUp | CursorDown | CursorPrev | CursorNext deriving (Show)

data EditAction c = InsertBefore c | InsertAfter c | DeleteBefore | DeleteAfter deriving (Show)

class WrappingEditor a c | a -> c where
  moveCursor :: a -> CursorMove -> a
  editData :: a -> EditAction c -> a
  getAllContent :: a -> [c]


data ViewChange u = AbsoluteFrame u | RelativeFrame u | UpdateOffset u deriving (Show)

class LineViewport a u | a -> u where
  changeView :: a -> ViewChange u -> a
  seekInView :: a -> u -> a
  getCursor :: a -> Maybe u


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

instance RenderedLines (LineViewer c) (LineViewer c) where
  getAllLines = (:[])


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

instance RenderedLines (LineEditor c) (LineViewer c) where
  getAllLines = getAllLines . getFullLine


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

instance RenderedLines (ParaEditor c) (LineViewer c) where
  getAllLines (ParaEditor bs l as p) =
    concat (reverse $ map getAllLines bs) ++
    getAllLines l ++
    concat (map getAllLines as)


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

instance RenderedLines (ParaViewer c) (LineViewer c) where
  getAllLines = concat . map getAllLines . pvLines


data FixedCharSize =
  FixedCharSize {
    fcsWidth :: Int,
    fcsHeight :: Int
  }
  deriving (Show)


class Show a => FixedLineBreaker a c where
  breakLine :: a -> Int -> [c] -> (LineViewer c,[c])
  joinLines :: a -> [LineViewer c] -> [c]


data TextEditor c =
  forall a. FixedLineBreaker a c => TextEditor {
    teBefore :: [ParaViewer c],  -- Reversed.
    teEditing :: ParaEditor c,
    teAfter :: [ParaViewer c],
    teViewSize :: FixedCharSize,
    teViewOffset :: FixedCharSize,
    teWrap :: a
  }


instance Show c => Show (TextEditor c) where
  show (TextEditor bs e as s o w) =
    (concat $ map (++ "\n") $ map show $ reverse bs) ++
    show e ++ "\n" ++
    (concat $ map (++ "\n") $ map show as) ++
    "Size: " ++ show s ++ ", Corner: " ++ show o ++ ", Wrap: " ++ show w

newTextEditor :: FixedLineBreaker a c => FixedCharSize -> a -> [UnparsedPara c] -> TextEditor c
newTextEditor s wrap [] = newTextEditor s wrap [UnparsedPara []]
newTextEditor s@(FixedCharSize w h) wrap (p:ps) =
  TextEditor {
    teBefore = [],
    teEditing = newParaEditor (breakLine wrap w) p,
    teAfter = map (newParaViewer (breakLine wrap w)) ps,
    teViewSize = s,
    teViewOffset = FixedCharSize 0 0,
    teWrap = wrap
  }

flattenEditorContents :: TextEditor c -> [UnparsedPara c]
flattenEditorContents (TextEditor bs e as _ _ w) =
  (map (flattenToPara w) $ reverse bs) ++
  [flattenToPara w e] ++
  (map (flattenToPara w) as)


splitAllLines :: ([c] -> (l,[c])) -> [c] -> [l]
splitAllLines f [] = []
splitAllLines f cs = let (l,cs2) = f cs in l:(splitAllLines f cs2)

flattenToPara :: (FixedLineBreaker a c, RenderedLines b (LineViewer c)) => a -> b -> UnparsedPara c
flattenToPara w = UnparsedPara . joinLines w . getAllLines


breakParagraphs = map UnparsedPara . lines

data LineBreakPolicy = FixedLineWidth deriving Show

instance FixedLineBreaker LineBreakPolicy Char where
  breakLine FixedLineWidth n cs = (LineViewer $ take n cs,drop n cs)
  joinLines _ = concat . map lvLine

main = do
  contents <- readFile "testdata.txt"
  let paras = breakParagraphs contents
  let editor = newTextEditor (FixedCharSize 5 10) FixedLineWidth paras
  putStrLn $ show editor
  putStrLn $ show $ flattenEditorContents editor
