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


data LineBreak = SymbolBreak | WhitespaceBreak | BrokenWord | ParaBreak deriving (Show)

data LineViewer c =
  LineViewer {
    lvLine :: [c],
    lvBreak :: LineBreak
  }
  deriving (Show)

newLineViewer :: [c] -> LineBreak -> LineViewer c
newLineViewer cs b = editor where
  editor = LineViewer {
      lvLine = cs,
      lvBreak = b
    }

emptyLineViewer :: LineViewer c
emptyLineViewer = newLineViewer [] ParaBreak

instance RenderedLines (LineViewer c) (LineViewer c) where
  getAllLines = (:[])


data LineEditor c =
  LineEditor {
    leCharsBefore :: [c],  -- Reversed.
    leCharsAfter :: [c],
    leBreak :: LineBreak,
    lePosition :: Int,
    leSize :: Int
  }
  deriving (Show)

newLineEditor :: LineViewer c -> LineEditor c
newLineEditor (LineViewer cs b) =
  LineEditor {
    leCharsBefore = [],
    leCharsAfter = cs,
    leBreak = b,
    lePosition = 0,
    leSize = length cs
  }

getLineChar :: LineEditor c -> Int
getLineChar = lePosition

seekLineChar :: Int -> LineEditor c -> LineEditor c
seekLineChar n e@(LineEditor bs as b p s)
  | p < n && not (null as) = seekLineChar n (LineEditor (head as:bs) (tail as) b (p+1) s)
  | p > n && not (null bs) = seekLineChar n (LineEditor (tail bs) (head bs:as) b (p-1) s)
  | otherwise = e

getFullLine :: LineEditor c -> LineViewer c
getFullLine (LineEditor bs as b _ _) = LineViewer (reverse bs ++ as) b

getLineSize :: LineEditor c -> Int
getLineSize = leSize

instance RenderedLines (LineEditor c) (LineViewer c) where
  getAllLines = getAllLines . getFullLine


data UnparsedPara c =
  UnparsedPara {
    upData :: [c]
  }
  deriving (Show)


splitAllLines :: ([c] -> (l,[c])) -> [c] -> [l]
splitAllLines _ [] = []
splitAllLines f cs = let (l,cs2) = f cs in l:(splitAllLines f cs2)


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
  nonempty [] = [emptyLineViewer]
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
  getAllLines = nonempty . concat . map getAllLines . pvLines where
    nonempty [] = [emptyLineViewer]
    nonempty ls = ls


data FixedCharSize =
  FixedCharSize {
    fcsWidth :: Int,
    fcsHeight :: Int
  }
  deriving (Show)


class Show a => FixedLineBreaker a c where
  breakLine :: a -> Int -> [c] -> (LineViewer c,[c])
  renderLine :: a -> LineViewer c -> [c]
  joinLines :: a -> [LineViewer c] -> [c]


data ViewBuffer c =
  ViewBuffer {
    vbAbove :: [LineViewer c],  -- Reversed.
    vbBelow :: [LineViewer c]
  }
  deriving (Show)

newViewBuffer :: [LineViewer c] -> [LineViewer c] -> ViewBuffer c
newViewBuffer la lb =
  ViewBuffer {
    vbAbove = la,
    vbBelow = lb
  }

shiftViewFocus :: Int -> ViewBuffer c -> ViewBuffer c
shiftViewFocus n v@(ViewBuffer la lb)
  | n < 0 && not (null la) = shiftViewFocus (n+1) (ViewBuffer (tail la) (head la:lb))
  | n > 0 && not (null lb) = shiftViewFocus (n-1) (ViewBuffer (head lb:la) (tail lb))
  | otherwise = v

replaceLinesBelow :: Int -> [LineViewer c] -> ViewBuffer c -> ViewBuffer c
replaceLinesBelow n ls (ViewBuffer la lb) = ViewBuffer la (ls ++ drop n lb)

-- TODO: This will fail if the split is out of view.
getViewLines :: Int -> Int -> ViewBuffer c -> [LineViewer c]
getViewLines na nb (ViewBuffer la lb) = reverse (take na la) ++ take nb lb


data TextEditor c =
  forall a. FixedLineBreaker a c => TextEditor {
    teBefore :: [UnparsedPara c],  -- Reversed.
    teEditing :: ParaEditor c,
    teAfter :: [UnparsedPara c],
    teBuffer :: ViewBuffer c,
    teViewSize :: FixedCharSize,
    teViewOffset :: FixedCharSize,
    teEditCursor :: FixedCharSize,  -- Relative to top-left of view.
    teWrap :: a
  }

instance Show c => Show (TextEditor c) where
  show (TextEditor bs e as _ s o c w) =
    (concat $ map (++ "\n") $ map show $ reverse bs) ++
    show e ++ "\n" ++
    (concat $ map (++ "\n") $ map show as) ++
    "Size: " ++ show s ++ ", Corner: " ++ show o ++ ", Cursor: " ++ show c ++ ", Wrap: " ++ show w

newTextEditor :: FixedLineBreaker a c => FixedCharSize -> a -> [UnparsedPara c] -> TextEditor c
newTextEditor s wrap [] = newTextEditor s wrap [UnparsedPara []]
newTextEditor s@(FixedCharSize w h) wrap (p:ps) =
  TextEditor {
    teBefore = [],
    teEditing = editing,
    teAfter = ps,
    teBuffer = buffer,
    teViewSize = s,
    teViewOffset = FixedCharSize 0 0,
    teEditCursor = FixedCharSize 0 0,
    teWrap = wrap
  } where
    editing = newParaEditor (breakLine wrap w) p
    rendered = map (newParaViewer (breakLine wrap w)) ps
    buffer = newViewBuffer [] (getAllLines editing ++ concat (map getAllLines rendered))

renderEditorContents :: TextEditor c -> [[c]]
renderEditorContents (TextEditor _ _ _ b (FixedCharSize _ h) _ (FixedCharSize _ cy) w) = contents where
  contents = map (renderLine w) $ getViewLines na nb b
  na = cy
  nb = h-cy

flattenEditorContents :: TextEditor c -> [UnparsedPara c]
flattenEditorContents (TextEditor bs e as _ _ _ _ w) = reverse bs ++ [flattenToPara w e] ++ as


flattenToPara :: (FixedLineBreaker a c, RenderedLines b (LineViewer c)) => a -> b -> UnparsedPara c
flattenToPara w = UnparsedPara . joinLines w . getAllLines


-- Testing stuff -->

breakParagraphs = map UnparsedPara . lines

data LineBreakPolicy = FixedLineWidth deriving Show

breakToLine :: [Char] -> [Char] -> (LineViewer Char,[Char])
breakToLine ls [] = (LineViewer ls ParaBreak,[])
breakToLine ls cs
  | isWhitespace (head cs) = (LineViewer ls WhitespaceBreak,tail cs)
  | isWhitespace (last ls) = (LineViewer (init ls) WhitespaceBreak,cs)
  | isLetter (head cs) && isLetter (last ls) = (LineViewer ls BrokenWord,cs)
  | otherwise = (LineViewer ls SymbolBreak,cs) where
    isWhitespace c = c == ' '
    isLetter c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')

unrenderLine :: LineViewer Char -> [Char]
unrenderLine (LineViewer cs WhitespaceBreak) = cs ++ " "
unrenderLine (LineViewer cs _)               = cs

instance FixedLineBreaker LineBreakPolicy Char where
  breakLine FixedLineWidth n cs = breakToLine front rest where
    front = take n cs
    rest = drop n cs
  renderLine _ (LineViewer cs BrokenWord) = cs ++ "-"
  renderLine _ (LineViewer cs _)          = cs
  joinLines _ = concat . map unrenderLine

main = do
  contents <- readFile "testdata.txt"
  let paras = breakParagraphs contents
  let editor = newTextEditor (FixedCharSize 20 10) FixedLineWidth paras
  putStrLn $ show editor
  putStr $ unlines $ map upData $ flattenEditorContents editor
  putStr $ unlines $ renderEditorContents editor
