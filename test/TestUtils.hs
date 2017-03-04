
module TestUtils where

import Language.Pads.Parser
import Language.Pads.Syntax
import Text.Parsec.Error

import Test.HUnit.Base
import qualified Test.QuickCheck as QC
import Control.Monad (liftM)

testParse :: String -> Either ParseError [PadsDecl]
testParse = parsePadsDecls "test_src" 0 0


labeledAssert :: String -> Bool -> Test
labeledAssert l = TestLabel l . TestCase . assert

resOk :: QC.Result -> Bool
resOk QC.Success {} = True
resOk _ = False

propertyTests :: QC.Testable prop => [(String, prop)] -> IO Bool
propertyTests tests =
  and <$> mapM (liftM resOk
                . (\(tag, test) ->
                     putStrLn tag >>
                     QC.quickCheckWithResult
                        (QC.stdArgs { QC.maxSuccess = 50
                                    , QC.maxSize = 20 }) test))
                tests
