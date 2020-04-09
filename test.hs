{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Safe #-}

import Test.Common (runTests)
import qualified Test.Line as TestLine (allTests)
import qualified Test.Para as ParaLine (allTests)


main = do
  runTests "Line Tests" TestLine.allTests
  runTests "Para Tests" ParaLine.allTests
