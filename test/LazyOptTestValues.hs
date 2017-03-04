{-# LANGUAGE TemplateHaskell, QuasiQuotes, ScopedTypeVariables #-}

-- | This has to be factored out into a different module for
--   Template Haskell reasons
module LazyOptTestValues where

import Language.Pads.Padsc
import Language.Pads.Syntax
import Language.Haskell.TH (Exp(..), Lit(..))
import Language.Pads.LazyOpt
import Language.Haskell.TH.Syntax (Strict(..), mkName, runQ)
import TestUtils
import Test.HUnit.Base

--
-- utils
--

fixedTestTy :: String -> PadsTy -> ((PadsTy, SkipStrategy) -> Bool) -> IO Test
fixedTestTy l ty p = do
  res <- runQ $ ssPadsTy ty
  return . TestLabel l . TestCase . assert . p . optimise $ res

fixedTestDecl :: String -> PadsDecl
              -> ((PadsDecl, SkipStrategy) -> Bool) -> IO Test
fixedTestDecl l ty p = do
  res <- runQ $ ssPadsDecl ty
  return . TestLabel l . TestCase . assert . p $ res

fixedTestData :: String -> PadsData
              -> ((PadsData, SkipStrategy) -> Bool) -> IO Test
fixedTestData l ty p = do
  res <- runQ $ ssPadsData ty
  return . TestLabel l . TestCase . assert . p $ res


chkP :: String -> (String -> (a,String)) -> (a -> Bool) -> String -> IO Test
chkP label parser pred source =
  return . TestLabel label . TestCase . assert . pred . fst . parser $ source

eqSkipped :: (Eq a, PadsMD meta) => a -> (a, meta) -> Bool
eqSkipped x (x1, meta) = x == x1 && (skipped . get_md_header) meta

eqForced :: (Eq a, PadsMD meta) => a -> (a, meta) -> Bool
eqForced x (x1, meta) = x == x1 && (not . skipped . get_md_header) meta

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


[pads|
     -- Basic `force` and `defer` test
     type IntAlias = Int
     skin ForceInt for IntAlias = force
     skin DeferInt for IntAlias = defer

     type TupleFWPrefix = (StringFW 10, ' ', Int)
     -- skin DeferTupleFWPrefix for TupleFWPrefix = defer
|]
