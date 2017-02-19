{-# LANGUAGE TemplateHaskell, QuasiQuotes, ScopedTypeVariables #-}
module LazyOptTest where

import Language.Pads.LazyAccessors
import Language.Pads.LazyOpt
import Language.Pads.Syntax
import Language.Pads.RegExp
import Language.Pads.CoreBaseTypes

import LazyOptTestValues

import Test.HUnit.Text
import Test.HUnit.Base

test :: IO Counts
test = runTestTT $ TestList [
    fixedTestTy "fixedWidthString" fixedWidthString isFixedWidth
  , fixedTestTy "fixedLengthArray" fixedLengthArray isFixedWidth
  , fixedTestTy "mulitLetterLiteral" multiLetterLiteral isFixedWidth
  , fixedTestTy "mulitLitRE" multiLitRE (not . isFixedWidth)
  , fixedTestTy "fixedTuple" fixedTuple isFixedWidth
  , fixedTestTy "nonFixedTuple" nonFixedTuple
      ((==[SSFixed 3, SSNone, SSFixed 45]) . map snd . (\(SSSeq x) -> x) . snd)
  , TestLabel "fixedPrims" $
        TestList (map (TestCase . assert . isFixedWidth . ssPadsTy) fixedPrims)
  , fixedTestDecl "typeAlias" typeAlias isFixedWidth
  , fixedTestData "singleBranchDataDecl" singleBranchDataDecl isFixedWidth
  , fixedTestData "twoBranchSameWidth" twoBranchSameWidth isFixedWidth
  , fixedTestData "twoBranchDifferentWidth" twoBranchDifferentWidth (not . isFixedWidth)

  , TestLabel "optFixed" . TestCase . assert
    . (==SSFixed 10) . snd . optimise $
       (PTyvar "BOGUS",
        SSSeq (zip (repeat (PTyvar "BOGUS")) [SSFixed 3, SSFixed 4, SSFixed 3]))

  , TestLabel "optSeq" . TestCase . assert
    . (==[SSNone, SSFixed 4, SSNone, SSFixed 5])
    . map snd . (\(SSSeq x) -> x) . snd . optimise $
         (PTyvar "BOGUS",
            SSSeq (zip (repeat (PTyvar "BOGUS"))
                   [SSSeq [(PTyvar "BOGUS", SSNone), (PTyvar "BOGUS", SSFixed 4)],
                    SSNone, SSSeq [(PTyvar "BOGUS", SSFixed 5)]]))

  , TestLabel "shouldBeThree" . TestCase . assert . (==3) $ shouldBeThree
  ]

fixedTestTy :: String -> PadsTy -> ((PadsTy, SkipStrategy) -> Bool) -> Test
fixedTestTy l ty p =
  TestLabel l . TestCase . assert . p . optimise . ssPadsTy $ ty

fixedTestDecl :: String -> PadsDecl -> ((PadsDecl, SkipStrategy) -> Bool) -> Test
fixedTestDecl l ty p = TestLabel l . TestCase . assert . p . ssPadsDecl $ ty

fixedTestData :: String -> PadsData -> ((PadsData, SkipStrategy) -> Bool) -> Test
fixedTestData l ty p = TestLabel l . TestCase . assert . p . ssPadsData $ ty

$(genLazyAccessor fixedTupleAnn "aFixedWidthTuple")
