{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Safe #-}

import System.Directory
import System.IO

import Test.Common (runTests)
import qualified Test.Document as TestDocument (allTests)
import qualified Test.Line as TestLine (allTests)
import qualified Test.Para as TestPara (allTests)


main = do
  cwd <- getCurrentDirectory
  hPutStrLn stderr $ "Running from " ++ cwd
  runTests "Line Tests" TestLine.allTests
  runTests "Para Tests" TestPara.allTests
  runTests "Document Tests" TestDocument.allTests
