{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Safe #-}

module Test.Base (
  BreakExact,
  LineBreak(..),
  breakExact,
  checkCondition,
  checkConditions,
  composeActions,
  newLine,
  repeatAction,
  runTests,
  testFail,
  testPass,
) where

import Control.Applicative ((<|>))
import Control.Monad (when)
import Data.Maybe
import System.IO

import Line
import Para
import Parser


data LineBreak = LineBreak | AlternateBreak deriving (Enum,Eq,Ord,Show)

data BreakExact = BreakExact Int deriving (Show)

breakExact :: BreakExact
breakExact = BreakExact 0

instance FixedFontParser BreakExact Char LineBreak where
  setLineWidth _ w = BreakExact w
  breakParas _ = map UnparsedPara . lines
  joinParas _ = unlines . map upText
  breakLines _ [] = [emptyLine]
  breakLines (BreakExact w) cs
    | w < 2 = [VisibleLine cs LineBreak]
    | otherwise = breakOrEmpty cs where
      breakOrEmpty [] = []
      breakOrEmpty cs = line:(breakOrEmpty rest) where
        line = VisibleLine (take w cs) LineBreak
        rest = drop w cs
  joinLines _ = concat . map vlText
  renderLine _ = vlText

newLine :: String -> VisibleLine Char LineBreak
newLine cs = VisibleLine cs LineBreak

composeActions :: [a -> a] -> a -> a
composeActions = foldr (flip (.)) id

repeatAction :: Int -> (a -> a) -> a -> a
repeatAction n f = composeActions (replicate n f)

checkConditions :: [(Bool, String)] -> IO (Maybe String)
checkConditions = fmap (foldr (<|>) Nothing) . sequence . map (uncurry checkCondition)

checkCondition :: Bool -> String -> IO (Maybe String)
checkCondition True _  = testPass
checkCondition False m = testFail m

testPass :: IO (Maybe String)
testPass = return Nothing

testFail :: String -> IO (Maybe String)
testFail m = return (Just m)

runTests :: String -> [(String,IO (Maybe String))] -> IO ()
runTests setName tests = do
  results <- fmap catMaybes $ sequence $ map runTest $ zip [1..] tests
  hPutStrLn stderr $ resultSummary results
  when (not $ null results) $ hPutStr stderr $ unlines results
  where
    resultSummary rs = "*** " ++ setName ++ ": " ++
                       show (length tests - length rs) ++ " passed, " ++
                       show (length rs) ++ " failed ***"
    runTest (n,(name,test)) = do
      result <- test
      case result of
           Just m -> return (Just $ "Test " ++ show n ++ " (" ++ name ++ "): " ++ m)
           Nothing -> return Nothing
