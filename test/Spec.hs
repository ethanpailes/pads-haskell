
module Main where

import qualified Examples.First as First (test)
import qualified Examples.Binary as Binary (test)
import qualified LazyOptTest (test)
import qualified Examples.Regression as Regression (test)

-- these take a really long time to compile, and often cause stack to
-- get real cozy with the oom killer :-(
-- import qualified Examples.AI as AI (test)
-- import qualified Examples.VCard as VCard (test)

main :: IO ()
main = do
  putStrLn "First.test"
  print <$> First.test
  putStrLn "Binary.test"
  print <$> Binary.test
  putStrLn "LazyOptTest.test"
  print <$> LazyOptTest.test
  putStrLn "Regression.test"
  print <$> Regression.test
  -- AI.test
  -- VCard.test
  return ()
