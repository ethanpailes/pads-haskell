{-# LANGUAGE TemplateHaskell #-}

{-|
Module      : Language.Pads.LazyAccessors
Description : Functions which access a specific field in a pads type
Copyright   : (c) Kathleen Fisher, 2017
Maintainer  : kathleen.fisher@email.com
Stability   : experimental
Portability : POSIX

This module defines code generators for functions which read
the skip strategies emited during codegen by the
`Language.Pads.LazyOpt` module, and use them to construct a
skip plan to access the right members.
-}
module Language.Pads.LazyAccessors where

import Language.Pads.MetaData
import qualified Language.Pads.Source as S
import Language.Pads.PadsParser
import Language.Pads.CoreBaseTypes
import Language.Pads.Source
import Language.Pads.BaseTypes
import Language.Pads.LazyOpt
import Language.Pads.Syntax
import Language.Pads.CodeGen (genParseTy)
import Data.Monoid ((<>))
import Prelude as P

import Language.Haskell.TH hiding (compE)
import Control.Exception

genLazyAccessor :: (PadsTy, SkipStrategy) -> String -> Q [Dec]
genLazyAccessor ty baseName =
  case ty of
    (PTuple taus, _) ->
      concat <$> mapM (genTupleAccessor ty baseName) [1..(length taus)]


genTupleAccessor :: (PadsTy, SkipStrategy) -> String -> Int -> Q [Dec]
genTupleAccessor (PTuple taus, SSSeq skips) baseName n =
  let tupleLength = length taus in
  assert (tupleLength == length skips && tupleLength >= n) $ do
    input <- newName "input"
    skipFun <- fuseSS (PTuple (P.take (n - 1) taus), SSSeq (P.take (n - 1) skips))
    parseFun <- asParser . P.head . drop (n - 1) $ taus
    let funNameJustSkip = mkName (baseName ++ ('_' : show n) ++ "_S")
        funNameSkipAndParse = mkName (baseName ++ ('_' : show n) ++ "_SP")
        args = [VarP input]
        interestingInput = skipFun `AppE` (VarE input)
    return $ [ FunD funNameJustSkip [Clause args (NormalB interestingInput) []]
             , ValD (VarP funNameSkipAndParse)
                    (NormalB (parseFun `compE` (VarE funNameJustSkip))) []
             ]


compE :: Exp -> Exp -> Exp
compE e1 e2 = UInfixE e1 (VarE (mkName ".")) e2


--
-- General Lazy Accessor Utilities
--

-- | return an expression of type (String -> String)
fuseSS :: (PadsTy, SkipStrategy) -> Q Exp
fuseSS (ty, skipStrat) =
  case skipStrat of
    (SSFixed n) -> [| snd . S.takeBytes n |]
    (SSSeq []) -> [| id |]
    (SSSeq [s]) -> fuseSS s
    (SSSeq (s:ss@((nextTy, _):_))) ->
      [| $(fuseSS (nextTy, SSSeq ss)) . $(fuseSS s) |]
    SSNone -> asParser ty >>= \p -> [| snd . p |]

newtype Unimplimented = Unimplimented String
                      deriving(Show)
instance Exception Unimplimented

-- | A little wrapper around `genParseTy` that attempts to make
--   use of _parseS functions which have already been generated
asParser :: PadsTy -> Q Exp
asParser ty =
  case getParserName ty of
    (Just parseFun) -> parseFun >>= \n -> [| (VarE n) |]
    Nothing -> [| \source -> fst ( $(genParseTy ty) # source) |]

getParserName :: PadsTy -> Maybe (Q Name)
-- getParserName (PExpression _) = Just . return . mkName $ "BOGUS"
getParserName _ = Nothing
