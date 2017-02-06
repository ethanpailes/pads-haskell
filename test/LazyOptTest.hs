
module LazyOptTest where

import Language.Haskell.TH (Exp(..), Lit(..))
import Language.Haskell.TH.Syntax (Strict(..))
import Language.Pads.Syntax
import Language.Pads.LazyOpt
import Language.Pads.Parser
import Text.Parsec.Error

import Test.HUnit.Text
import Test.HUnit.Base

test :: IO Counts
test = runTestTT $ TestList [
    fixedTestTy "fixedWidthString" fixedWidthString
  , fixedTestTy "fixedLengthArray" fixedLengthArray
  , fixedTestTy "mulitLetterLiteral" multiLetterLiteral
  , TestLabel "fixedPrims" $
        TestList (map (TestCase . assert . isFixedWidth . ssPadsTy) fixedPrims)
  , fixedTestDecl "typeAlias" typeAlias
  , fixedTestData "singleBranchDataDecl" singleBranchDataDecl
  , fixedTestData "twoBranchSameWidth" twoBranchSameWidth
  , TestLabel "twoBranchDifferentWidth" .
        TestCase . assert . not . isFixedWidth
        . ssPadsData $ twoBranchDifferentWidth
  ]

fixedTestTy :: String -> PadsTy -> Test
fixedTestTy l = TestLabel l . TestCase . assert . isFixedWidth . ssPadsTy

fixedTestDecl :: String -> PadsDecl -> Test
fixedTestDecl l = TestLabel l . TestCase . assert . isFixedWidth . ssPadsDecl

fixedTestData :: String -> PadsData -> Test
fixedTestData l = TestLabel l . TestCase . assert . isFixedWidth . ssPadsData

--
-- GHCI testing values
--

testParse :: String -> Either ParseError [PadsDecl]
testParse = parsePadsDecls "test_src" 0 0

fixedWidthString :: PadsTy
fixedWidthString =
  (PApp [PTycon ["StringFW"]] (Just (LitE (IntegerL 10))))

fixedLengthArray :: PadsTy
fixedLengthArray =
  (PList (PApp [PTycon ["StringFW"]] (Just (LitE (IntegerL 9))))
   Nothing (Just (LLen (LitE (IntegerL 15)))))

multiLetterLiteral :: PadsTy
multiLetterLiteral = mll
  where Right [PadsDeclType _ _ _ mll] =
          testParse "type F = 'abc'"

fixedPrims :: [PadsTy]
fixedPrims = map (\c -> PTycon [c]) ["Char", "Digit"]

-- Declarations

typeAlias :: PadsDecl
typeAlias = PadsDeclType "Foo" [] Nothing (PTycon ["Digit"])

-- Data Types

singleBranchDataDecl :: PadsData
singleBranchDataDecl = (PUnion [BRecord "Foo"
          [(Just "s1", (NotStrict, PApp [PTycon ["StringFW"]]
                           (Just (LitE (IntegerL 10)))),Nothing),
            (Nothing,(NotStrict,PExpression (LitE (CharL ' '))),Nothing),
            (Just "s2",(NotStrict,PApp [PTycon ["StringFW"]]
                         (Just (LitE (IntegerL 2)))),Nothing)] Nothing])

twoBranchSameWidth :: PadsData
twoBranchSameWidth = r
  where Right [PadsDeclData _ _ _ r _] = testParse
                      "data D = D { foo :: StringFW 4, ' ', baz :: StringFW 13 }\
                      \       | B { 'xyz', bar :: StringFW 15 }"

twoBranchDifferentWidth :: PadsData
twoBranchDifferentWidth = r
  where Right [PadsDeclData _ _ _ r _] = testParse
                      "data D = D { foo :: StringFW 90, ' ', baz :: StringFW 13 }\
                      \       | B { 'xyz', bar :: StringFW 15 }"
