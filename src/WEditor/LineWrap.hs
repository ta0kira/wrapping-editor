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

-- | Line-wrapping implementations. (See 'FixedFontParser' for custom wrapping.)

{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Safe #-}

module WEditor.LineWrap (
  BreakWords,
  LineBreak,
  WordSplitter(..),
  breakExact,
  breakWords,
  lazyHyphen,
  lineBreakEnd,
  lineBreakHyphen,
  lineBreakSimple,
  noHyphen,
) where

import Control.Applicative ((<|>))
import Control.Monad (when)

import WEditor.Base


-- | Line break type for a single paragraph line.
data LineBreak = ParagraphEnd | SimpleBreak | HyphenatedWord deriving (Eq,Ord)

-- | The line is at the end of the paragraph.
lineBreakEnd :: LineBreak
lineBreakEnd = ParagraphEnd

-- | The line is nothing special.
lineBreakSimple :: LineBreak
lineBreakSimple = SimpleBreak

-- | The line ends with a hyphenated word.
lineBreakHyphen :: LineBreak
lineBreakHyphen = HyphenatedWord

data NoSplit c = NoSplit deriving (Show)

-- | Wrapping policy that breaks at exactly the viewer width.
breakExact :: BreakWords c
breakExact = breakWords NoSplit

instance WordSplitter (NoSplit c) c

-- | Word-splitting operations for use with 'BreakWords'.
class WordSplitter a c | a -> c where
  -- | Determine where to break a word.
  --
  --   * The splitter can refuse to process the word by returning 'Nothing'.
  --   * The segment sizes must provide space for a hyphen if 'appendHyphen'
  --     extends the line.
  --   * Once the word has been split up by 'BreakWords', the segments are
  --     processed as follows:
  --
  --       1. The last segment is prepended to the next line to be parsed. This
  --          means that if the word is not split, it gets deferred to the
  --          next line.
  --       2. If there are more segments, the first is appended to the current
  --          line being parsed.
  --       3. All remaining segments are put on separate lines between the
  --          current and next lines.
  splitWord :: a
            -> Int         -- ^ Space available on the first line.
            -> Int         -- ^ Space available on new lines.
            -> [c]         -- ^ The word to break.
            -> Maybe [Int] -- ^ List of segment sizes.
  splitWord _ _ _ _ = Nothing
  -- | Predicate for characters that should be treated as a part of a word.
  isWordChar :: a -> c -> Bool
  isWordChar _ _ = False
  -- | Predicate for detecting whitespace between words.
  isWhitespace :: a -> c -> Bool
  isWhitespace _ _ = False
  -- | Append the canonical hyphen character to show word breaks.
  appendHyphen :: a -> [c] -> [c]
  appendHyphen _ = id

-- | Wrapping policy that breaks lines based on words. Use 'breakWords' to
--   construct a new value.
data BreakWords c = forall a. (Show a, WordSplitter a c) => BreakWords Int a

-- | Wrapping policy that breaks lines based on words.
breakWords :: (Show a, WordSplitter a c) => a -> BreakWords c
breakWords = BreakWords 0

data NoHyphen c = NoHyphen deriving (Show)

-- | Avoids splitting words unless they are longer than a single line.
noHyphen :: WordChar c => NoHyphen c
noHyphen = NoHyphen

data LazyHyphen c = LazyHyphen deriving (Show)

-- | Hyphenates words using simple aesthetics, without dictionary awareness.
lazyHyphen :: (WordChar c, HyphenChar c) => LazyHyphen c
lazyHyphen = LazyHyphen


-- Private below here.

instance Show LineBreak where
  show ParagraphEnd   = "lineBreakEnd"
  show SimpleBreak    = "lineBreakSimple"
  show HyphenatedWord = "lineBreakHyphen"

instance Show (BreakWords c) where
  show (BreakWords w s) =
    "breakWords { width: " ++ show w ++
               ", split: " ++ show s ++ " }"

instance DefaultBreak LineBreak where
  defaultBreak = lineBreakEnd

instance WordChar c => WordSplitter (NoHyphen c) c where
  splitWord _ k w _ = if k < w then Just [] else Nothing
  isWordChar _ = defaultIsWordChar
  isWhitespace _ = defaultIsWhitespace

instance (WordChar c, HyphenChar c) => WordSplitter (LazyHyphen c) c where
  splitWord _ k w cs
    | w < 4 || k > w          = Nothing
    | k >= length cs || k < 3 = Just []
    | otherwise = Just $ (k-1):(replicate count size) where
        size = w-1
        remainder = length cs-(k-1)
        -- Uses remainder-2 because the last line needs no hyphen. This is the
        -- same as iteratively breaking off w-1 until the remainder is < w+1.
        count = (remainder-2) `div` size
  isWordChar _ = defaultIsWordChar
  isWhitespace _ = defaultIsWhitespace
  appendHyphen _ = (++[defaultHyphen])

instance FixedFontParser (BreakWords c) c LineBreak where
  setLineWidth (BreakWords _ s) w = BreakWords w s
  breakLines (BreakWords w s) = breakAllLines w s
  renderLine (BreakWords w _) (VisibleLine ParagraphEnd cs)
    | w < 1 = cs
    | otherwise = take w cs
  renderLine (BreakWords _ s) (VisibleLine SimpleBreak cs) =
    reverse $ dropWhile (isWhitespace s) $ reverse cs
  renderLine (BreakWords _ s) (VisibleLine HyphenatedWord cs) = appendHyphen s cs
  tweakCursor (BreakWords w _) (VisibleLine ParagraphEnd _)
    | w < 1 = id
    | otherwise = min w
  tweakCursor (BreakWords _ s) (VisibleLine SimpleBreak cs) = max 0 . min (total-post) where
    post = length $ takeWhile (isWhitespace s) $ reverse cs
    total = length cs
  tweakCursor _ (VisibleLine HyphenatedWord cs) = id

breakAllLines :: WordSplitter a c => Int -> a -> [c] -> [VisibleLine c LineBreak]
breakAllLines _ _ [] = [emptyLine]
breakAllLines w s cs
  | w < 1 = [VisibleLine lineBreakEnd cs]
  | otherwise = breakOrEmpty cs where
      breakOrEmpty cs = let (Just ls) = handleSplit (reverse $ take w cs) (drop w cs) in ls
      handleSplit line rest =
        tryWord line rest <|>
        trySpaces line rest <|>
        lineDefault line rest
      lineDefault []  _ = Just []
      lineDefault ls [] = Just [VisibleLine lineBreakEnd (reverse ls)]
      lineDefault ls rs = Just $ VisibleLine lineBreakSimple (reverse ls):(breakOrEmpty rs)
      tryWord ls@(l:_) rs@(r:_) | isWordChar s l && isWordChar s r = newLines where
        newLines = do
          breaks <- splitWord s (length wordFront) w word
          -- Safety fallback for misbehaving splitters.
          when (null breaks && length wordFront == w) Nothing
          return $ case breaks of
                        []     -> VisibleLine lineBreakSimple (reverse ls2):(breakOrEmpty (word ++ rs2))
                        (b:bs) -> VisibleLine lineBreakHyphen (reverse ls2 ++ take b word):(hyphenate (drop b word) bs)
        ls2 = dropWhile (isWordChar s) ls
        rs2 = dropWhile (isWordChar s) rs
        wordFront = reverse $ takeWhile (isWordChar s) ls
        wordBack = takeWhile (isWordChar s) rs
        word = wordFront ++ wordBack
        hyphenate word bs | null word || null bs = breakOrEmpty (word ++ rs2)
        hyphenate word (b:bs) = (VisibleLine lineBreakHyphen (take b word)):(hyphenate (drop b word) bs)
      tryWord _ _ = Nothing
      trySpaces ls rs@(r:_) | isWhitespace s r = newLines where
        ls' = reverse ls ++ takeWhile (isWhitespace s) rs
        rs' = dropWhile (isWhitespace s) rs
        newLines
          | null rs'  = Just [VisibleLine lineBreakEnd ls']
          | otherwise = Just $ (VisibleLine lineBreakSimple ls'):(breakOrEmpty rs')
      trySpaces _ _ = Nothing
