
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
    fixedTestTy "fixedWidthString" fixedWidthString isFixedWidth
  , fixedTestTy "fixedLengthArray" fixedLengthArray isFixedWidth
  , fixedTestTy "mulitLetterLiteral" multiLetterLiteral isFixedWidth
  , fixedTestTy "fixedTuple" fixedTuple isFixedWidth
  , fixedTestTy "nonFixedTuple" nonFixedTuple ((==SSSeq [SSFixed 3, SSNone, SSFixed 45]) . getAnn)
  , TestLabel "fixedPrims" $
        TestList (map (TestCase . assert . isFixedWidth . ssPadsTy) fixedPrims)
  , fixedTestDecl "typeAlias" typeAlias isFixedWidth
  , fixedTestData "singleBranchDataDecl" singleBranchDataDecl isFixedWidth
  , fixedTestData "twoBranchSameWidth" twoBranchSameWidth isFixedWidth
  , fixedTestData "twoBranchDifferentWidth" twoBranchDifferentWidth (not . isFixedWidth)
  ]

fixedTestTy :: String -> PadsTy -> (PadsTyAnn SkipStrategy -> Bool) -> Test
fixedTestTy l ty p = TestLabel l . TestCase . assert . p . ssPadsTy $ ty

fixedTestDecl :: String -> PadsDecl -> (PadsDeclAnn SkipStrategy -> Bool) -> Test
fixedTestDecl l ty p = TestLabel l . TestCase . assert . p . ssPadsDecl $ ty

fixedTestData :: String -> PadsData -> (PadsDataAnn SkipStrategy -> Bool) -> Test
fixedTestData l ty p = TestLabel l . TestCase . assert . p . ssPadsData $ ty

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

fixedTuple :: PadsTy
fixedTuple = ty
  where Right [PadsDeclType _ _ _ ty] =
          testParse "type F = ('abc', 'bar', StringFW 45)"

nonFixedTuple :: PadsTy
nonFixedTuple = ty
  where Right [PadsDeclType _ _ _ ty] =
          testParse "type F = ('abc', StringC ',', StringFW 45)"

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
