
module Main where

import qualified Examples.First as First (test)
import qualified Examples.Binary as Binary (test)
import qualified LazyOptTest (test)

-- these take a really long time to compile, and often cause stack to
-- get real cozy with the oom killer :-(
-- import qualified Examples.AI as AI (test)
-- import qualified Examples.VCard as VCard (test)

main :: IO ()
main = do
  print <$> First.test
  print <$> Binary.test
  print <$> LazyOptTest.test
  -- AI.test
  -- VCard.test
  return ()
