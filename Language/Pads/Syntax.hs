{-# LANGUAGE DeriveDataTypeable, MultiParamTypeClasses,
    TypeSynonymInstances, FlexibleInstances, TemplateHaskell #-}

{-
** *********************************************************************
*     (c) 2011                                                         *
*         Kathleen Fisher <kathleen.fisher@gmail.com>                  *
*         John Launchbury <john.launchbury@gmail.com>                  *
*                                                                      *
************************************************************************
-}



module Language.Pads.Syntax where

import Data.Generics
import Language.Haskell.TH
import Language.Haskell.TH.Syntax as THS
import Language.Haskell.TH.Lift (deriveLiftMany)
import Language.Haskell.TH.Instances

--
-- A possible way to make this a little less verbose would be the Uniplate
-- library, but now that the code is written, why make it slower?
--

data PadsDecl = PadsDeclType   String [String] (Maybe Pat) PadsTy
              | PadsDeclData   String [String] (Maybe Pat) PadsData [QString]
              | PadsDeclNew    String [String] (Maybe Pat) BranchInfo [QString]
              | PadsDeclObtain String [String] PadsTy Exp
              -- | arg1: the name of skin,
              --   arg2: the PADS type to apply the skin to,
              --   arg3: the pattern to apply
              | PadsDeclSkin
                  String
                  (Maybe String)
                  PadsSkinPat
   deriving (Eq, Data, Typeable, Show)


-- | A pads skin tells PADS which values to force during
--   parsing. One can be declared like
--   `skin SkinName for SomePadsType = <pattern>`
--   or just with
--   `skin SkinName = <pattern>`
--   if you don't want to apply the skin to a type just yet
data PadsSkinPat = PSForce -- ^ Force the type. Terminal.
                 | PSDefer -- ^ Defer the type for later parsing. Terminal.

                 -- pattern stuff
                 | PSConP String [PadsSkinPat] -- ^ constructor patterns
                 | PSRecP String [(String, PadsSkinPat)] -- ^ record patterns
                 | PSTupleP [PadsSkinPat] -- ^ tuple patterns

                 -- | A reference to a previously defined skin
                 | PSSkin String
   deriving (Eq, Data, Typeable, Show)


data PadsTy = PConstrain Pat PadsTy Exp
            | PTransform PadsTy PadsTy Exp
            | PList PadsTy (Maybe PadsTy) (Maybe TermCond)
            | PPartition PadsTy Exp
            | PValue Exp PadsTy
            | PApp [PadsTy] (Maybe Exp)
            | PTuple [PadsTy]
            | PExpression Exp
            | PTycon QString
            | PTyvar String
   deriving (Eq, Data, Typeable, Show)

data TermCond = LTerm PadsTy
              | LLen Exp
  deriving (Eq, Data, Typeable, Show)

data PadsData = PUnion [BranchInfo]
              | PSwitch Exp [(Pat,BranchInfo)]
  deriving (Eq, Data, Typeable, Show)

data BranchInfo = BRecord String [FieldInfo] (Maybe Exp)
                | BConstr String [ConstrArg] (Maybe Exp)
  deriving (Eq, Data, Typeable, Show)


type FieldInfo = (Maybe String, ConstrArg, Maybe Exp)

type ConstrArg = (Strict, PadsTy)

type QString = [String]  -- qualified names

--
-- Allow annotated pads ast nodes to be lifed into compiletime values
--
$(deriveLiftMany [''PadsDecl, ''PadsTy, ''TermCond, ''PadsData,
                  ''BranchInfo, ''PadsSkinPat])


hasRep :: PadsTy -> Bool
hasRep (PExpression l)   = False
hasRep (PTycon ["EOF"])  = False
hasRep (PTycon ["EOR"])  = False
hasRep (PTycon ["Void"]) = False
hasRep ty                 = True

qName :: QString -> String
qName [n] = n
qName (n:ms) = n ++ "." ++ qName ms

