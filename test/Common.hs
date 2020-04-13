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

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Safe #-}

module Common (
  checkCondition,
  checkConditions,
  composeActions,
  endLine,
  hyphenLine,
  innerLine,
  repeatAction,
  runTests,
  testFail,
  testPass,
) where

import Control.Applicative ((<|>))
import Control.Monad (when)
import Data.Maybe (catMaybes)
import System.IO (hPutStr,hPutStrLn,stderr)

import LineWrap
import Base.Line
import Base.Parser


endLine :: String -> VisibleLine Char LineBreak
endLine = VisibleLine lineBreakEnd

innerLine :: String -> VisibleLine Char LineBreak
innerLine = VisibleLine lineBreakSimple

hyphenLine :: String -> VisibleLine Char LineBreak
hyphenLine = VisibleLine lineBreakHyphen

composeActions :: [a -> a] -> a -> a
composeActions = foldr (flip (.)) id

repeatAction :: Int -> (a -> a) -> a -> a
repeatAction n f = composeActions (replicate n f)

checkConditions :: [(String,Bool)] -> IO (Maybe String)
checkConditions = fmap (foldr (<|>) Nothing) . sequence . map (uncurry checkCondition)

checkCondition :: String -> Bool -> IO (Maybe String)
checkCondition _ True  = testPass
checkCondition m False = testFail m

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
