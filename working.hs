{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Safe #-}

import Data.List (intercalate)


class RenderedLines a l | a -> l where
  getVisibleLines :: a -> [l]


data CursorMove = CursorUp | CursorDown | CursorPrev | CursorNext deriving (Eq,Show)

data CursorStatus = CursorAtFront | CursorAtTop | CursorInMiddle | CursorAtBottom | CursorAtBack deriving (Eq,Show)

data EditDirection = EditBefore | EditAfter deriving (Eq,Show)

data EditAction c = EditInsert c | EditDelete deriving (Show)

class LineBasedEditor a c u | a -> c u where
  moveCursor :: a -> CursorMove -> a
  getCursorPos :: a -> u
  getCursorStatus :: a -> CursorStatus
  editData :: a -> EditAction c -> EditDirection -> a
  breakPara :: a -> EditDirection -> a
  getFlatDocument :: a -> [c]


data LineBreak = SymbolBreak | WhitespaceBreak | BrokenWord | ParaBreak deriving (Eq,Show)

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
  getVisibleLines = (:[])


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

getFullLine :: LineEditor c -> LineViewer c
getFullLine (LineEditor bs as b _ _) = LineViewer (reverse bs ++ as) b

-- TODO:
seekLineChar :: Int -> LineEditor c -> LineEditor c
seekLineChar k2 e@(LineEditor bs as b k n)
  | k2 < k && not (null as) = seekLineChar n (LineEditor (head as:bs) (tail as) b (k+1) n)
  | k2 > n && not (null bs) = seekLineChar n (LineEditor (tail bs) (head bs:as) b (k-1) n)
  -- TODO: This allows the cursor to be past the end. Make sure it's bounded by
  -- n when move/edit operations happen.
  | otherwise = LineEditor bs as b k2 n

moveLineCursorPrev :: LineEditor c -> LineEditor c
moveLineCursorPrev e@(LineEditor bs as b k n)
  | null bs = e
  | otherwise = LineEditor (tail bs) (head bs:as) b (k-1) n

moveLineCursorNext :: LineEditor c -> LineEditor c
moveLineCursorNext e@(LineEditor bs as b k n)
  | null as = e
  | otherwise = LineEditor (head as:bs) (tail as) b (k+1) n

getLineCursorStatus :: LineEditor c -> CursorStatus
getLineCursorStatus e@(LineEditor as bs _ k n)
  | k == 0 || null bs = CursorAtFront
  | k >= n || null as = CursorAtBack
  | otherwise = CursorInMiddle

editLineData :: EditAction c -> EditDirection -> LineEditor c -> LineEditor c
editLineData (EditInsert x) d e@(LineEditor bs as b k n)
  | d == EditBefore = LineEditor (x:bs) as    b (k+1) (n+1)
  | d == EditAfter  = LineEditor bs    (x:as) b k     (n+1)
editLineData EditDelete d e@(LineEditor bs as b k n)
  | getLineCursorStatus e /= CursorAtTop    && d == EditBefore = LineEditor (tail bs) as        b (k-1) (n-1)
  | getLineCursorStatus e /= CursorAtBottom && d == EditAfter  = LineEditor bs        (tail as) b (k-1) (n-1)
  | otherwise = e

breakLineAtCursor :: LineEditor c -> (LineViewer c, LineViewer c)
breakLineAtCursor (LineEditor bs as b _ _) = (newLineViewer (reverse bs) ParaBreak,newLineViewer as b)

mergeEditedLines :: Maybe (LineViewer c) -> LineEditor c -> Maybe (LineViewer c) -> LineEditor c
mergeEditedLines b l a = addBefore b $ addAfter a l where
  addBefore (Just (LineViewer cs _)) (LineEditor bs as b k n) =
    LineEditor (reverse cs ++ bs) as b (k + length cs) (n + length cs)
  addBefore _ l = l
  addAfter (Just (LineViewer cs b)) (LineEditor bs as _ k n) =
    LineEditor bs (as ++ cs) b k (n + length cs)
  addAfter _ l = l

instance RenderedLines (LineEditor c) (LineViewer c) where
  getVisibleLines = (:[]) . getFullLine


data UnparsedPara c =
  UnparsedPara {
    upData :: [c]
  }
  deriving (Show)


splitAllLines :: ([c] -> (l,[c])) -> [c] -> [l]
splitAllLines _ [] = []
splitAllLines f cs = let (l,cs2) = f cs in l:(splitAllLines f cs2)


data FixedCharSize =
  FixedCharSize {
    fcsHorizontal :: Int,
    fcsVertical :: Int
  }
  deriving (Eq,Show)


data ParaEditor c =
  ParaEditor {
    peLinesBefore :: [LineViewer c],  -- Reversed.
    peLineCurrent :: LineEditor c,
    peLinesAfter :: [LineViewer c],
    peCursorLine :: Int,
    peTotalLines :: Int
  }
  deriving (Show)

newParaEditor :: ([c] -> (LineViewer c,[c])) -> UnparsedPara c -> ParaEditor c
newParaEditor wrap (UnparsedPara cs) = editor where
  editor = ParaEditor {
      peLinesBefore = [],
      peLineCurrent = newLineEditor first,
      peLinesAfter = rest,
      peCursorLine = 0,
      peTotalLines = length rest + 1
    }
  (first:rest) = nonempty (splitAllLines wrap cs)
  nonempty [] = [emptyLineViewer]
  nonempty ls = ls

-- TODO: Cursor up/down only works properly with fixed-width viewing => should
-- ParaEditor and LineEditor be tied to FixedCharSize?
-- TODO: Cursor up/down doesn't preserve end-of-line when moving from long to
-- short to long lines, e.g., going from 10 to 5 to 10 leaves the cursor at 5.
moveParaCursor :: CursorMove -> ParaEditor c -> ParaEditor c
moveParaCursor CursorUp e@(ParaEditor bs l as k n)
  | k <= 0 || null bs = ParaEditor [] l as 0 n  -- Corrects k and bs, just in case.
  | otherwise = ParaEditor (tail bs) editor (getFullLine l:as) (k-1) n where
    editor = seekLineChar (lePosition l) $ newLineEditor (head bs)
moveParaCursor CursorDown e@(ParaEditor bs l as k n)
  | k >= n || null as = ParaEditor bs l [] 0 n  -- Corrects k and as, just in case.
  | otherwise = ParaEditor (getFullLine l:bs) editor (tail as) (k+1) n where
    editor = seekLineChar (lePosition l) $ newLineEditor (head as)
moveParaCursor CursorPrev e@(ParaEditor bs l as k n)
  | getLineCursorStatus l /= CursorAtTop = (ParaEditor bs (moveLineCursorPrev l) as k n)
  | otherwise = ParaEditor (tail bs) editor' (getFullLine l:as) (k-1) n where
    editor = newLineEditor (head bs)
    editor' = seekLineChar (leSize editor) editor  -- Seek the end of the line.
moveParaCursor CursorNext e@(ParaEditor bs l as k n)
  | getLineCursorStatus l /= CursorAtBottom = (ParaEditor bs (moveLineCursorNext l) as k n)
  | otherwise = ParaEditor (getFullLine l:bs) editor (tail as) (k+1) n where
    editor = newLineEditor (head as)

getParaCursorStatus :: ParaEditor c -> CursorStatus
getParaCursorStatus (ParaEditor _ l _ k n)
  | k == 0 = if getLineCursorStatus l == CursorAtFront
                then CursorAtFront
                else CursorAtTop
  | k == n = if getLineCursorStatus l == CursorAtBack
                then CursorAtBack
                else CursorAtBottom
  | otherwise = CursorInMiddle

editParaData :: ([LineViewer c] -> [c]) ->  ([c] -> (LineViewer c,[c])) -> EditAction c -> EditDirection -> ParaEditor c -> ParaEditor c
editParaData join wrap i d (ParaEditor bs l as k n) = process i where
  process (EditInsert _) = reparse $ ParaEditor bs (editLineData i d l) as k n
  process EditDelete = reparse editor where
    (b,bs2) = takeHead bs
    (a,as2) = takeHead as
    l2 = mergeEditedLines a l b
    k2 = k - (if null bs then 0 else 1)
    n2 = n - (if null bs then 0 else 1) - (if null as then 0 else 1)
    editor = (ParaEditor bs2 (editLineData i d l2) as2 k2 n2)
    takeHead cs = if null cs
                     then (Nothing,cs)
                     else (Just $ head cs,tail cs)
  reparse (ParaEditor bs l as k n) = moveBy position editor where
    position = lePosition l
    editor = ParaEditor bs (newLineEditor first) rest k (length rest + k + 1)
    (first:rest) = splitAllLines wrap $ join (getFullLine l:as)
    moveBy k e
      | k > 0 = moveBy (k-1) $ moveParaCursor CursorNext e
      | otherwise = e

breakParaAtCursor :: ([LineViewer c] -> [c]) -> ParaEditor c -> (UnparsedPara c, UnparsedPara c)
breakParaAtCursor join (ParaEditor bs l as k n) =
  (UnparsedPara $ join $ reverse $ before:bs,UnparsedPara $ join $ after:as) where
    (before,after) = breakLineAtCursor l

instance RenderedLines (ParaEditor c) (LineViewer c) where
  getVisibleLines (ParaEditor bs l as _ _) =
    concat (reverse $ map getVisibleLines bs) ++
    getVisibleLines l ++
    concat (map getVisibleLines as)


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
  getVisibleLines = nonempty . concat . map getVisibleLines . pvLines where
    nonempty [] = [emptyLineViewer]
    nonempty ls = ls


class Show a => FixedLineBreaker a c where
  breakLine :: a -> Int -> [c] -> (LineViewer c,[c])
  renderLine :: a -> LineViewer c -> [c]
  joinLines :: a -> [LineViewer c] -> [c]
  joinParas :: a -> [UnparsedPara c] -> [c]


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
    teEditCursor :: FixedCharSize,  -- Relative to top-left of view.
    teWrap :: a
  }

instance Show c => Show (TextEditor c) where
  show (TextEditor bs e as _ s c w) =
    (concat $ map (++ "\n") $ map show $ reverse bs) ++
    show e ++ "\n" ++
    (concat $ map (++ "\n") $ map show as) ++
    "Size: " ++ show s ++ ", Cursor: " ++ show c ++ ", Wrap: " ++ show w

newTextEditor :: FixedLineBreaker a c => FixedCharSize -> a -> [UnparsedPara c] -> TextEditor c
newTextEditor s wrap [] = newTextEditor s wrap [UnparsedPara []]
newTextEditor s@(FixedCharSize w h) wrap (p:ps) =
  TextEditor {
    teBefore = [],
    teEditing = editing,
    teAfter = ps,
    teBuffer = buffer,
    teViewSize = s,
    teEditCursor = FixedCharSize 0 0,
    teWrap = wrap
  } where
    editing = newParaEditor (breakLine wrap w) p
    rendered = map (newParaViewer (breakLine wrap w)) ps
    buffer = newViewBuffer [] (getVisibleLines editing ++ concat (map getVisibleLines rendered))

instance RenderedLines (TextEditor c) [c] where
  getVisibleLines (TextEditor _ _ _ b (FixedCharSize _ vy) (FixedCharSize _ cy) w) = contents where
    contents = map (renderLine w) $ getViewLines na nb b
    na = cy
    nb = vy-cy

instance LineBasedEditor (TextEditor c) c FixedCharSize where
  moveCursor ea@(TextEditor bs e as b s c w) m = process (getParaCursorStatus e) m where
    process status CursorUp
      | status == CursorAtTop || status == CursorAtFront = ea  -- TODO: Move to same pos in last line of prev para.
    process status CursorDown
      | status == CursorAtBottom || status == CursorAtBack = ea  -- TODO: Move to same pos in first line of next para.
    process CursorAtFront CursorPrev = ea
    process CursorAtBack CursorNext = ea
    -- TODO: Update cursor position.
    process _ _ = TextEditor bs (moveParaCursor m e) as (shiftView m b) s c w where
    shiftView CursorUp   = shiftViewFocus (-1)
    shiftView CursorDown = shiftViewFocus 1
    shiftView _          = id
--   getCursorPos :: a -> u
--   getCursorStatus :: a -> CursorStatus
--   editData :: a -> EditAction c -> EditDirection -> a
--   breakPara :: a -> EditDirection -> a
--   getFlatDocument :: a -> [c]


flattenToPara :: (FixedLineBreaker a c, RenderedLines b (LineViewer c)) => a -> b -> UnparsedPara c
flattenToPara w = UnparsedPara . joinLines w . getVisibleLines


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
  joinParas _ = concat . map (++ "\n") . map upData

main = do
  contents <- readFile "testdata.txt"
  let paras = breakParagraphs contents
  let editor = newTextEditor (FixedCharSize 20 10) FixedLineWidth paras
  putStrLn $ show editor
--   putStr $ getAllContent editor
  putStr $ unlines $ getVisibleLines editor
