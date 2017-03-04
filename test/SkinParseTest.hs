
module SkinParseTest where

import Text.Parsec (ParseError)
import TestUtils
import Test.HUnit.Text
import Test.HUnit.Base
import Language.Pads.Syntax
import Language.Pads.Parser
import Language.Pads.Pretty()
import PropTesting()
import qualified Text.Parsec.Prim as P

import qualified Text.PrettyPrint.Mainland as PP

test :: IO Counts
test = do
  qcRes <- propTests
  runTestTT $ TestList [
      labeledAssert "quickCheckTests" qcRes
    , labeledAssert "parse_tuple_1"
        (testParse "skin Foo for Bar = (force, defer)"
        ==Right [PadsDeclSkin "Foo" (Just "Bar") (PSTupleP [PSForce, PSDefer])])
    , labeledAssert "parse_tuple_2 (no apply)"
       (testParse "skin Foo = (defer, force)"
        ==Right [PadsDeclSkin "Foo" Nothing (PSTupleP [PSDefer, PSForce])])
    , labeledAssert "parse_naked_force"
       (testParse "skin Foo = force"
        ==Right [PadsDeclSkin "Foo" Nothing PSForce])
    , labeledAssert "parse_naked_defer"
       (testParse "skin Foo for Baz = defer"
        ==Right [PadsDeclSkin "Foo" (Just "Baz") PSDefer])

    , labeledAssert "parse_naked_defer_parens"
       (testParse "skin Foo for Baz = ((defer))"
        ==Right [PadsDeclSkin "Foo" (Just "Baz") PSDefer])

    , labeledAssert "parse_con_1"
       (testParse "skin Foo = SomeConstructor (SomeOtherConstructor force defer)"
        ==Right [PadsDeclSkin "Foo" Nothing
                (PSConP ["SomeConstructor"] [PSConP ["SomeOtherConstructor"]
                                          [PSForce, PSDefer]])])

    , labeledAssert "parse_rec_1"
        (rec_1 == Right [PadsDeclSkin "Foo" Nothing
                (PSRecP ["SomeRecCon"] [
                  ("aField", PSTupleP [PSForce, PSConP ["SomeCon"] [PSDefer]])
                , ("anotherField", PSDefer) ])])

    , labeledAssert "skin_1"
       (testParse "skin Foo = <SomeSkin>"
        ==Right [PadsDeclSkin "Foo" Nothing (PSSkin ["SomeSkin"])])
    ]

propTests :: IO Bool
propTests = propertyTests [
    ("prop_SkinParse", prop_SkinParse)
  ]

prop_SkinParse :: PadsSkinPat -> Bool
prop_SkinParse pat = Right pat == parsed
  where pp = PP.pretty 100 (PP.ppr pat)
        parsed = P.parse parseSkinPat "prop_SkinParse" pp

rec_1 :: Either ParseError [PadsDecl]
rec_1 = testParse . unlines $ [
          "skin Foo = SomeRecCon {"
        , "aField = (force, SomeCon defer)"
        , ", anotherField = (defer) }"
        ]
