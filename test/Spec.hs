
module Main where

import qualified Examples.First as First (test)
import qualified Examples.Binary as Binary (test)
import qualified LazyOptTest (test)
import qualified Examples.Regression as Regression (test)
import qualified SkinParseTest (test)

import Test.HUnit.Base
import System.Exit

-- these take a really long time to compile, and often cause stack to
-- get real cozy with the oom killer :-(
-- import qualified Examples.AI as AI (test)
-- import qualified Examples.VCard as VCard (test)

main :: IO ()
main = do
  let tests = sequence [
                testSuite "First.test" First.test
              , testSuite "Binary.test" Binary.test
              , testSuite "LazyOptTest.test" LazyOptTest.test
              , testSuite "Regression.test" Regression.test
              , testSuite "SkinParseTest.test" SkinParseTest.test
              ]
  ok <- and <$> tests
  exitWith $ if ok then ExitSuccess else ExitFailure 1

testSuite :: String -> IO Counts -> IO Bool
testSuite suiteName test = do
  putStrLn suiteName
  count <- test
  return $ countOk count

countOk :: Counts -> Bool
countOk Counts { failures = n } | n > 0 = False
countOk _ = True
