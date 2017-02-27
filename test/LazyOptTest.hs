{-# LANGUAGE TemplateHaskell, QuasiQuotes, ScopedTypeVariables #-}
module LazyOptTest where

import Language.Pads.LazyAccessors
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
test = runTestTT . TestList $ $(ListE <$> (sequence $ [
    fixedTestTy "fixedWidthString" fixedWidthString [| isFixedWidth |]
  , fixedTestTy "fixedLengthArray" fixedLengthArray [| isFixedWidth |]
  , fixedTestTy "mulitLetterLiteral" multiLetterLiteral [| isFixedWidth |]
  , fixedTestTy "mulitLitRE" multiLitRE [| (not . isFixedWidth) |]
  , fixedTestTy "nonFixedTuple" nonFixedTuple
      [| ((==[SSFixed 3, SSNone, SSFixed 45])
          . map snd . (\(SSSeq x) -> x) . snd) |]
  , do { tests <- mapM (\(i, t) ->
                         fixedTestTy ("test"++show i) t [| isFixedWidth |])
                  (zip [0..] fixedPrims)
       ; [| TestLabel "fixedPrims" $ TestList $(return $ ListE tests) |]
       }
  , fixedTestDecl "typeAlias" typeAlias [| isFixedWidth |]
  , fixedTestData "singleBranchDataDecl" singleBranchDataDecl [| isFixedWidth |]
  , fixedTestData "twoBranchSameWidth" twoBranchSameWidth [| isFixedWidth |]
  , fixedTestData "twoBranchDifferentWidth"
        twoBranchDifferentWidth [| (not . isFixedWidth) |]
  , [| TestLabel "optFixed" . TestCase . assert
    . (==SSFixed 10) . snd . optimise $
       (PTyvar "BOGUS",
        SSSeq (zip (repeat (PTyvar "BOGUS")) [SSFixed 3, SSFixed 4, SSFixed 3]))
       |]
  , [| TestLabel "optSeq" . TestCase . assert
    . (==[SSNone, SSFixed 4, SSNone, SSFixed 5])
    . map snd . (\(SSSeq x) -> x) . snd . optimise $
         (PTyvar "BOGUS",
            SSSeq (zip (repeat (PTyvar "BOGUS"))
                   [SSSeq [(PTyvar "BOGUS", SSNone), (PTyvar "BOGUS", SSFixed 4)],
                    SSNone, SSSeq [(PTyvar "BOGUS", SSFixed 5)]]))
     |]

  , [| TestLabel "shouldBeThree" . TestCase . assert . (==3) $ shouldBeThree |]
  ]))

$(genLazyAccessor fixedTupleAnn "aFixedWidthTuple")



