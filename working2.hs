{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Safe #-}


-- Viewers.

data LineBreakType = ParaBreak | SpaceBreak | BrokenWord | TokenBreak deriving (Eq,Ord,Show)

data VisibleLine c =
  VisibleLine {
    vlText :: [c],
    vlWidth :: Int,
    vlBreak :: LineBreakType
  }
  deriving (Show)

data StaticPara c =
  UnparsedPara {
    upText :: [c]
  } |
  VisibleParaBefore {
    vpaLines :: [VisibleLine c]  -- Reversed.
  } |
  VisibleParaAfter {
    vpbLines :: [VisibleLine c]
  }
  deriving (Show)


-- Editors.

data EditAction c = InsertText [c] | DeleteText Int deriving (Show)

data EditDirection = EditBefore | EditAfter deriving (Eq,Ord,Show)

data MoveDirection = MoveUp | MoveDown | MovePrev | MoveNext deriving (Eq,Ord,Show)

data EditingLine c =
  EditingLine {
    elTextBefore :: [c],  -- Reversed.
    elTextAfter :: [c],
    elCursor :: Int,  -- TODO: Allow this to be past the end.
    elWidth :: Int,
    elBreak :: LineBreakType
  }
  deriving (Show)

data EditingPara c =
  EditingPara {
    epBefore :: [VisibleLine c],  -- Reversed.
    epEditing :: EditingLine c,
    epAfter :: [VisibleLine c],
    epLine :: Int,
    epHeight :: Int
  }
  deriving (Show)

data EditingDocument c =
  EditingDocument {
    edBefore :: [StaticPara c],  -- Reversed.
    edEditing :: EditingPara c,
    edAfter :: [StaticPara c],
    edWidth :: Int,
    edHeight :: Int,
    edViewOffset :: Int
  }
  deriving (Show)


-- Parsing.

class FixedFontParser a c | a -> c where
  setLineWidth :: a -> Int -> a
  breakParas :: a -> [c] -> [StaticPara c]
  joinParas :: a -> [StaticPara c] -> [c]
  breakLines :: a -> [c] -> [VisibleLine c]
  joinLines :: a -> [VisibleLine c] -> [c]
  renderLine :: a -> VisibleLine c -> [c]


-- Line Actions.

emptyLine :: VisibleLine c
emptyLine = VisibleLine [] 0 ParaBreak

editLine :: VisibleLine c -> EditingLine c
editLine (VisibleLine cs w b) = EditingLine [] cs 0 w b

viewLine :: EditingLine c -> VisibleLine c
viewLine (EditingLine bs as _ w b) = VisibleLine (reverse bs ++ as) w b

getLineCursor :: EditingLine c -> Int
getLineCursor = elCursor

setLineCursor :: Int -> EditingLine c -> EditingLine c
setLineCursor k e@(EditingLine bs as c w b)
  | k < c && not (null bs) = setLineCursor k $ EditingLine (tail bs) (head bs:as) (c-1) w b
  | k > c && not (null as) = setLineCursor k $ EditingLine (head as:bs) (tail as) (c+1) w b
  | otherwise = e

splitLine :: EditingLine c -> (VisibleLine c,VisibleLine c)
splitLine (EditingLine bs as c w b) =
  (VisibleLine (reverse bs) (length bs) ParaBreak,
   VisibleLine as (length as) b)

lineCursorMovable :: MoveDirection -> EditingLine c -> Bool
lineCursorMovable MovePrev (EditingLine (_:_) _ _ _ _) = True
lineCursorMovable MoveNext (EditingLine _ (_:_) _ _ _) = True
lineCursorMovable _ _ = False

moveLineCursor :: MoveDirection -> EditingLine c -> EditingLine c
moveLineCursor MovePrev (EditingLine bs as c w b)
  | not (null bs) = (EditingLine (tail bs) (head bs:as) (c-1) w b)
moveLineCursor MoveNext (EditingLine bs as c w b)
  | not (null as) = (EditingLine (head as:bs) (tail as) (c+1) w b)
moveLineCursor _ l = l

setCursorFront :: EditingLine c -> EditingLine c
setCursorFront (EditingLine bs as c w b) = (EditingLine [] (reverse bs ++ as) 0 w b)

setCursorBack :: EditingLine c -> EditingLine c
setCursorBack (EditingLine bs as c w b) = (EditingLine (reverse as ++ bs) [] w w b)

modifyLine :: EditAction c -> EditDirection -> EditingLine c -> EditingLine c
modifyLine (InsertText cs) d (EditingLine bs as c w b) = revised where
  bs2 = if d == EditBefore
           then reverse cs ++ bs
           else bs
  as2 = if d == EditAfter
           then cs ++ as
           else as
  revised = EditingLine bs2 as2 (length bs2) (length bs2 + length as2) b
modifyLine (DeleteText k) d (EditingLine bs as c w b) = revised where
  bs2 = if d == EditBefore
           then drop k bs
           else bs
  as2 = if d == EditAfter
           then drop k as
           else as
  revised = EditingLine bs2 as2 (length bs2) (length bs2 + length as2) b


-- Paragraph Actions.

flattenPara :: FixedFontParser a c => a -> StaticPara c -> [c]
flattenPara parser (UnparsedPara cs) = cs
flattenPara parser (VisibleParaBefore ls) = reverse $ joinLines parser ls
flattenPara parser (VisibleParaAfter ls) = joinLines parser ls

parseParaBefore :: FixedFontParser a c => a -> StaticPara c -> StaticPara c
parseParaBefore parser para = VisibleParaBefore (reverse $ breakLines parser cs) where
  cs = flattenPara parser para

parseParaAfter :: FixedFontParser a c => a -> StaticPara c -> StaticPara c
parseParaAfter parser para = VisibleParaBefore (breakLines parser cs) where
  cs = flattenPara parser para

editPara :: FixedFontParser a c => a -> StaticPara c -> EditingPara c
editPara parser para = EditingPara [] (editLine line) after 0 (length after + 1) where
  (line:after) = nonempty $ breakLines parser $ flattenPara parser para
  nonempty [] = [emptyLine]
  nonempty ls = ls

viewPara :: FixedFontParser a c => a -> EditingPara c -> StaticPara c
viewPara parser (EditingPara bs l as _ _) = VisibleParaAfter ls where
  ls = reverse bs ++ [viewLine l] ++ as

getParaCursor :: EditingPara c -> (Int,Int)
getParaCursor (EditingPara _ l _ n _) = (getLineCursor l,n)

setParaCursor :: Int -> EditingPara c -> EditingPara c
setParaCursor k e@(EditingPara bs l as n h) = (EditingPara bs (setLineCursor k l) as n h)

splitPara :: EditingPara c -> (StaticPara c,StaticPara c)
splitPara (EditingPara bs l as _ _) =
  (VisibleParaBefore bs,VisibleParaAfter (viewLine l:as))

paraCursorMovable :: MoveDirection -> EditingPara c -> Bool
paraCursorMovable d (EditingPara bs l as _ _)
  | d == MoveUp   = not (null bs)
  | d == MoveDown = not (null as)
  | otherwise = lineCursorMovable d l

moveParaCursor :: MoveDirection -> EditingPara c -> EditingPara c
moveParaCursor d p@(EditingPara bs l as n h) = revised where
  revised
    | not (paraCursorMovable d p) = p
    | d == MoveUp   = setParaCursor (getLineCursor l) $ EditingPara (tail bs) (editLine $ head bs) (viewLine l:as) (n-1) h
    | d == MoveDown = setParaCursor (getLineCursor l) $ EditingPara (viewLine l:bs) (editLine $ head as) (tail as) (n+1) h
    | lineCursorMovable d l = EditingPara bs (moveLineCursor d l) as n h
    | d == MovePrev = setBack  $ moveParaCursor MoveUp   p
    | d == MoveNext = setFront $ moveParaCursor MoveDown p
  setBack  (EditingPara bs l as n h) = (EditingPara bs (setCursorBack  l) as n h)
  setFront (EditingPara bs l as n h) = (EditingPara bs (setCursorFront l) as n h)

reparseParaTail :: FixedFontParser a c => a -> EditingPara c -> EditingPara c
reparseParaTail parser (EditingPara bs l as n h) = moveBy offset revised where
  offset = getLineCursor l
  revised = EditingPara bs (editLine line) after n (n + length as + 1)
  (line:after) = breakLines parser $ joinLines parser (viewLine l:as)
  moveBy k
    | k > 0 = moveBy (k-1) . moveParaCursor MoveNext
    | otherwise = id

modifyPara :: FixedFontParser a c => a -> EditAction c -> EditDirection -> EditingPara c -> EditingPara c
modifyPara parser (InsertText cs) d p@(EditingPara bs l as n h) = p  -- TODO: Implement.
modifyPara parser (DeleteText k)  d p@(EditingPara bs l as n h) = p  -- TODO: Implement.
