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

-- | Line-wrapping implementations. (See FixedFontParser for custom wrapping.)

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Safe #-}

module LineWrap (
  BreakExact,
  BreakWords,
  LineBreak,
  WordSplitter,
  breakExact,
  breakWords,
  lazyHyphen,
  lineBreakEnd,
  lineBreakHyphen,
  lineBreakSimple,
  noHyphen,
) where

import Control.Applicative ((<|>))

import Base


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

-- | Wrapping policy that breaks at exactly the viewer width.
newtype BreakExact c = BreakExact Int

-- | Wrapping policy that breaks at exactly the viewer width.
breakExact :: BreakExact c
breakExact = BreakExact 0

-- | A function to split words, for use with 'BreakWords'.
--
--     * The word breaks must provide space for an additional hyphen character
--       to be rendered if the word continues to the next line, i.e., all but
--       the last item in the returned list should be one character shorter than
--       the available space.
--     * The splitter can refuse to process the word by returning an empty list.
--     * Returning the whole word without breaks will bump the word to the next
--       line. This must be avoided if the first line is full width, i.e., if
--       the first two arguments are the same.
type WordSplitter c = Int   -- ^ Space available on the first line.
                   -> Int   -- ^ Space available on new lines.
                   -> [c]   -- ^ The word to break.
                   -> [[c]] -- ^ Word splits.

-- | Wrapping policy that breaks lines based on words.
data BreakWords c = BreakWords Int (WordSplitter c)

-- | Wrapping policy that breaks lines based on words.
breakWords :: (WordChar c, HyphenChar c) => WordSplitter c -> BreakWords c
breakWords = BreakWords 0

-- | Skips word hyphenation, and just splits at the viewport width.
noHyphen :: WordSplitter c
noHyphen _ _ _ = []

-- | Hyphenates using simple aesthetics, without dictionary awareness.
lazyHyphen :: WordSplitter c
lazyHyphen k w cs
  | w < 4 || k > w = []
  | k >= length cs || k < 3 = [cs]
  | otherwise = (take (k-1) cs):(splitBy w $ drop (k-1) cs) where
    splitBy _ [] = []
    splitBy n cs
      | length cs > n = (take (n-1) cs):(splitBy n $ drop (n-1) cs)
      | otherwise = [cs]


-- Private below here.

instance Show LineBreak where
  show ParagraphEnd   = "lineBreakEnd"
  show SimpleBreak    = "lineBreakSimple"
  show HyphenatedWord = "lineBreakHyphen"

instance Show (BreakWords c) where
  show (BreakWords w _) = "breakWords { width: " ++ show w ++ " }"

instance Show (BreakExact c) where
  show (BreakExact w) = "breakExact { width: " ++ show w ++ " }"

instance DefaultBreak LineBreak where
  defaultBreak = lineBreakEnd

instance FixedFontParser (BreakExact c) c LineBreak where
  setLineWidth _ w = BreakExact w
  breakLines _ [] = [emptyLine]
  breakLines (BreakExact w) cs
    | w < 1 = [VisibleLine lineBreakEnd cs]
    | otherwise = breakOrEmpty cs where
      breakOrEmpty [] = []
      breakOrEmpty cs = continue (reverse $ take w cs) (drop w cs) where
        continue ls [] = [VisibleLine lineBreakEnd (reverse ls)]
        continue ls rs = (VisibleLine lineBreakSimple (reverse ls)):(breakOrEmpty rs)
  renderLine _ = vlText

instance (WordChar c, HyphenChar c) => FixedFontParser (BreakWords c) c LineBreak where
  setLineWidth (BreakWords _ f) w = BreakWords w f
  breakLines (BreakWords w f) = breakAllLines w f
  renderLine (BreakWords w _) (VisibleLine ParagraphEnd cs)
    | w < 1 = cs
    | otherwise = take w cs
  renderLine _ (VisibleLine SimpleBreak cs) = reverse $ trimLeadingSpaces $ reverse cs
  renderLine _ (VisibleLine HyphenatedWord cs) = cs ++ [hyphenChar]
  tweakCursor (BreakWords w _) (VisibleLine ParagraphEnd _)
    | w < 1 = id
    | otherwise = min w
  tweakCursor _ (VisibleLine SimpleBreak cs) = max 0 . min (total-post) where
    post = countLeadingSpaces $ reverse cs
    total = length cs
  tweakCursor _ (VisibleLine HyphenatedWord cs) = id

breakAllLines :: WordChar c => Int -> WordSplitter c -> [c] -> [VisibleLine c LineBreak]
breakAllLines _ _ [] = [emptyLine]
breakAllLines w f cs
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
      tryWord ls@(l:_) rs@(r:_) | isWordChar l && isWordChar r = newLines where
        ls2 = dropWhile isWordChar ls
        rs2 = dropWhile isWordChar rs
        wordFront = reverse $ takeWhile isWordChar ls
        wordBack = takeWhile isWordChar rs
        breaks = f (length wordFront) w (wordFront ++ wordBack)
        newLines
          -- Splitter refuses to deal with the word.
          | null breaks = Nothing
          -- If there is no split, move the whole word to the next line.
          | length breaks == 1 = handleSplit ls2 (head breaks ++ rs2)
          | otherwise = Just $ hyphenate ((reverse ls2 ++ head breaks):(tail breaks)) rs2
        hyphenate bs rs =
          map (VisibleLine lineBreakHyphen) (init bs) ++
          -- Last break goes with the rest of the next line.
          breakOrEmpty (last bs ++ rs)
      tryWord _ _ = Nothing
      trySpaces ls rs@(r:_) | isSpaceChar r = newLines where
        ls' = reverse ls ++ takeWhile isSpaceChar rs
        rs' = dropWhile isSpaceChar rs
        newLines
          | null rs'  = Just [VisibleLine lineBreakEnd ls']
          | otherwise = Just $ (VisibleLine lineBreakSimple ls'):(breakOrEmpty rs')
      trySpaces _ _ = Nothing