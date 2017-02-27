{-# LANGUAGE TemplateHaskell, QuasiQuotes, ScopedTypeVariables #-}

-- | This has to be factored out into a different module for
--   Template Haskell reasons
module LazyOptTestValues where

import Language.Pads.Padsc
import Language.Pads.Syntax
import Language.Haskell.TH (Exp(..), Lit(..), Q)
import Language.Pads.LazyOpt
import Language.Haskell.TH.Syntax (Strict(..), mkName)
import TestUtils

--
-- utils
--

import Test.HUnit.Base

-- : takes the label, the input type and the predicate function as an expression
--   returns a `Test` as an expression
fixedTestTy :: String -> PadsTy -> Q Exp -> Q Exp
fixedTestTy l ty p =
  [| TestLabel l . TestCase . assert . $p . optimise $ $(ssPadsTy ty) |]

-- : takes the label, the input type and the predicate function as an expression
--   returns a `Test` as an expression
fixedTestDecl :: String -> PadsDecl -> Q Exp -> Q Exp
fixedTestDecl l ty p =
  [| TestLabel l . TestCase . assert . $p $ $(ssPadsDecl ty) |]

-- : takes the label, the input type and the predicate function as an expression
--   returns a `Test` as an expression
fixedTestData :: String -> PadsData -> Q Exp -> Q Exp
fixedTestData l ty p =
  [| TestLabel l . TestCase . assert . $p $ $(ssPadsData ty) |]

--
-- test values
--

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

multiLitRE :: PadsTy
multiLitRE = mll
  where Right [PadsDeclType _ _ _ mll] =
          testParse "type F = '[ \\t]+'"

fixedPrims :: [PadsTy]
fixedPrims = map (\c -> PTycon [c]) ["Char", "Digit"]

[pads| type AFixedWidthTuple = ('abc', 'bar', StringFW 7) |]
fixedTuple :: PadsTy
fixedTuple = ty
  where (PadsDeclType _ _ _ ty, _) =
          pcg_METADATA_skipStrategy pads_CG_METADATA_AFixedWidthTuple
fixedTupleAnn :: (PadsTy, SkipStrategy)
fixedTupleAnn = (ty, ss)
  where (PadsDeclType _ _ _ ty, ss) =
          pcg_METADATA_skipStrategy pads_CG_METADATA_AFixedWidthTuple


fixedTuple' :: PadsTy
fixedTuple' = ty
  where Right [PadsDeclType _ _ _ ty] =
          testParse "type F = (Phex32FW 2, ',', Phex32FW 3)"

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


ssFunAdd :: SkipStrategy
ssFunAdd = let x = mkName "x"
               y = mkName "y"
            in (SSFun [x,y] (UInfixE (VarE x) (VarE (mkName "+")) (VarE y)))



-- A smoke test for the SSFun hack
shouldBeThree :: Int
shouldBeThree = $(applySSFun

                   -- the function to apply
                 (let x = mkName "x"
                      y = mkName "y"
                   in (SSFun [x,y] (UInfixE (VarE x)
                                    (VarE (mkName "+")) (VarE y))))

                 -- the arguments to the function
                 (ListE [LitE (IntegerL 1),LitE (IntegerL 2)]))
