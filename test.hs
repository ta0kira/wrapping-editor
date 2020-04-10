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

import System.Directory
import System.IO

import Test.Common (runTests)
import qualified Test.Document as TestDocument (allTests)
import qualified Test.Line as TestLine (allTests)
import qualified Test.LineWrap as TestLineWrap (allTests)
import qualified Test.Para as TestPara (allTests)


main = do
  cwd <- getCurrentDirectory
  hPutStrLn stderr $ "Running from " ++ cwd
  runTests "LineWrap Tests" TestLineWrap.allTests
  runTests "Line Tests" TestLine.allTests
  runTests "Para Tests" TestPara.allTests
  runTests "Document Tests" TestDocument.allTests
