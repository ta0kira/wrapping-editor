{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Safe #-}

import Test.Base (runTests)
import qualified Test.Line as TestLine (allTests)


main = do
  runTests "Line Tests" TestLine.allTests
