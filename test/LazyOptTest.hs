{-# LANGUAGE TemplateHaskell, QuasiQuotes, ScopedTypeVariables #-}
module LazyOptTest where

import Language.Pads.LazyOpt
import Language.Pads.Syntax
import Language.Pads.RegExp
import Language.Pads.CoreBaseTypes
import Language.Haskell.TH
import Language.Haskell.TH.Syntax

import Language.Haskell.Meta.Utils

import LazyOptTestValues

import Test.HUnit.Text
import Test.HUnit.Base
import Data.Data

showSrc :: (Data a, Ppr a) => Q a -> IO ()
showSrc x = do
  thing <- runQ x
  putStrLn $ pp thing

test :: IO Counts
test =
  TestList <$> (sequence [
      fixedTestTy "fixedWidthString" fixedWidthString isFixedWidth
    , fixedTestTy "fixedLengthArray" fixedLengthArray isFixedWidth
    , fixedTestTy "mulitLetterLiteral" multiLetterLiteral isFixedWidth
    , fixedTestTy "mulitLitRE" multiLitRE (not . isFixedWidth)
    , fixedTestTy "nonFixedTuple" nonFixedTuple
        ((==[SSFixed 3, SSNone, SSFixed 45])
            . map snd . (\(SSSeq x) -> x) . snd)
    , let tests = map (\(i, t) ->
                          fixedTestTy ("test"++show i) t isFixedWidth)
                    (zip [0..] fixedPrims)
        in TestLabel "fixedPrims" . TestList <$> sequence tests
    , fixedTestDecl "typeAlias" typeAlias isFixedWidth
    , fixedTestData "singleBranchDataDecl" singleBranchDataDecl isFixedWidth
    , fixedTestData "twoBranchSameWidth" twoBranchSameWidth isFixedWidth
    , fixedTestData "twoBranchDifferentWidth"
          twoBranchDifferentWidth (not . isFixedWidth)
    , TestLabel "optFixed" . TestCase . assert
      . (==SSFixed 10) . snd . optimise <$>
        return (PTyvar "BOGUS",
          SSSeq (zip (repeat (PTyvar "BOGUS")) [SSFixed 3, SSFixed 4, SSFixed 3]))
    , TestLabel "optSeq" . TestCase . assert
      . (==[SSNone, SSFixed 4, SSNone, SSFixed 5])
      . map snd . (\(SSSeq x) -> x) . snd . optimise <$>
          return (PTyvar "BOGUS",
              SSSeq (zip (repeat (PTyvar "BOGUS"))
                    [SSSeq [(PTyvar "BOGUS", SSNone),
                            (PTyvar "BOGUS", SSFixed 4)],
                      SSNone, SSSeq [(PTyvar "BOGUS", SSFixed 5)]]))

    , chkP "forceInt" 0 forceInt_parseFoldS (eqForced 19) "19"
    , chkP "deferInt" 0 deferInt_parseFoldS (eqSkipped 0) "19"
    -- , chkP "forceTupFWPrefix"
    --        forceTupleFWPrefix_parseS (eqForced (4,58)) "4 58"
    -- , chkP "deferTupFWPrefix"
    --        deferTupleFWPrefix_parseS (eqSkipped (0,0)) "4 58"
    -- , chkP "deferPrefixTupFWPrefix"
    --        deferPrefixTupleFWPrefix_parseS ((==(0,58)) . fst) "4 58"
    ]) >>= runTestTT
