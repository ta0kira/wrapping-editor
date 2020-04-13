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
--       to be rendered if the word continues to the next line.
--     * The splitter can refuse to process the word by returning 'Nothing'.
--     * Returning an empty list will bump the word to the next line. This must
--       be avoided if the first line is full width to avoid infinite recursion.
type WordSplitter c = Int         -- ^ Space available on the first line.
                   -> Int         -- ^ Space available on new lines.
                   -> [c]         -- ^ The word to break.
                   -> Maybe [Int] -- ^ List of break sizes.

-- | Wrapping policy that breaks lines based on words.
data BreakWords c = BreakWords Int (WordSplitter c)

-- | Wrapping policy that breaks lines based on words.
breakWords :: (WordChar c, HyphenChar c) => WordSplitter c -> BreakWords c
breakWords = BreakWords 0

-- | Avoids splitting words unless they are longer than a single line.
noHyphen :: WordSplitter c
noHyphen k w cs = if k < w then Just [] else Nothing

-- | Hyphenates using simple aesthetics, without dictionary awareness.
lazyHyphen :: WordSplitter c
lazyHyphen k w cs
  | w < 4 || k > w          = Nothing
  | k >= length cs || k < 3 = Just []
  | otherwise = Just $ (k-1):(replicate count size) where
      size = w-1
      remainder = length cs-(k-1)
      -- Uses remainder-2 because the last line needs no hyphen. This is the
      -- same as iteratively breaking off w-1 until the remainder is < w+1.
      count = (remainder-2) `div` size


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
        newLines = do
          breaks <- f (length wordFront) w word
          return $ case breaks of
                        []     -> VisibleLine lineBreakSimple (reverse ls2):(breakOrEmpty (word ++ rs2))
                        (b:bs) -> VisibleLine lineBreakHyphen (reverse ls2 ++ take b word):(hyphenate (drop b word) bs)
        ls2 = dropWhile isWordChar ls
        rs2 = dropWhile isWordChar rs
        wordFront = reverse $ takeWhile isWordChar ls
        wordBack = takeWhile isWordChar rs
        word = wordFront ++ wordBack
        hyphenate word bs | null word || null bs = breakOrEmpty (word ++ rs2)
        hyphenate word (b:bs) = (VisibleLine lineBreakHyphen (take b word)):(hyphenate (drop b word) bs)
      tryWord _ _ = Nothing
      trySpaces ls rs@(r:_) | isSpaceChar r = newLines where
        ls' = reverse ls ++ takeWhile isSpaceChar rs
        rs' = dropWhile isSpaceChar rs
        newLines
          | null rs'  = Just [VisibleLine lineBreakEnd ls']
          | otherwise = Just $ (VisibleLine lineBreakSimple ls'):(breakOrEmpty rs')
      trySpaces _ _ = Nothing
