{-# LANGUAGE DeriveDataTypeable, MultiParamTypeClasses,
    TypeSynonymInstances, FlexibleInstances #-}

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

--
-- The AST annotation technique used here is loosly inspired by this post:
-- https://mail.haskell.org/pipermail/haskell-cafe/2010-July/080842.html
-- the approach differes in that we don't want to break backwards compatability
-- with the old AST too badly.
--
-- A possible way to make this a little less verbose would be the Uniplate
-- library, but now that the code is written, why make it slower?
--


class Annotation a where
  getAnn :: a b -> b

-- | c is annotated as a
class Annotation a => Annotated c a where
  ann :: b -> c -> a b
  unAnn :: a b -> (b, c)

  viewC :: a b -> c
  viewC = snd . unAnn

  {-# MINIMAL ann, unAnn #-}



-- class Annotated a where
--   getAnn :: a b -> b

data PadsDeclAnn a =
    PadsDeclTypeAnn   a  String [String] (Maybe Pat) (PadsTyAnn a)
  | PadsDeclDataAnn   a  String [String] (Maybe Pat) (PadsDataAnn a) [QString]
  | PadsDeclNewAnn    a  String [String] (Maybe Pat) (BranchInfoAnn a) [QString]
  | PadsDeclObtainAnn a  String [String] (PadsTyAnn a) Exp
   deriving (Eq, Data, Typeable, Show)

unPadsDeclAnn :: PadsDeclAnn a -> (a, PadsDecl)
unPadsDeclAnn (PadsDeclTypeAnn   ann  x1 x2 x3 x4) =
  (ann, PadsDeclType x1 x2 x3 (viewC x4))
unPadsDeclAnn (PadsDeclDataAnn   ann  x1 x2 x3 x4 x5) =
  (ann, PadsDeclData x1 x2 x3 (vPadsData x4) x5)
unPadsDeclAnn (PadsDeclNewAnn    ann  x1 x2 x3 x4 x5) =
  (ann, PadsDeclNew x1 x2 x3 (viewC x4) x5)
unPadsDeclAnn (PadsDeclObtainAnn ann x1 x2 x3 x4) =
  (ann, PadsDeclObtain x1 x2 (viewC x3) x4)

instance Annotation PadsDeclAnn where
  getAnn = fst . unPadsDeclAnn

instance Annotated PadsDecl PadsDeclAnn where
  ann = annPadsDecl
  unAnn = unPadsDeclAnn

annPadsDecl :: a -> PadsDecl -> PadsDeclAnn a
annPadsDecl ann (PadsDeclType x0 x1 x2 x3)  =
  PadsDeclTypeAnn ann x0 x1 x2 (annPadsTy ann x3)
annPadsDecl ann (PadsDeclData x0 x1 x2 x3 x4)  =
  PadsDeclDataAnn ann x0 x1 x2 (annPadsData ann x3) x4
annPadsDecl ann (PadsDeclNew x0 x1 x2 x3 x4)  =
  PadsDeclNewAnn ann x0 x1 x2 (annBranchInfo ann x3) x4
annPadsDecl ann (PadsDeclObtain x0 x1 x2 x3)  =
  PadsDeclObtainAnn ann x0 x1 (annPadsTy ann x2) x3

data PadsDecl = PadsDeclType   String [String] (Maybe Pat) PadsTy
              | PadsDeclData   String [String] (Maybe Pat) PadsData [QString]
              | PadsDeclNew    String [String] (Maybe Pat) BranchInfo [QString]
              | PadsDeclObtain String [String] PadsTy Exp
   deriving (Eq, Data, Typeable, Show)


data PadsTyAnn a =
    PConstrainAnn a Pat (PadsTyAnn a) Exp
  | PTransformAnn a (PadsTyAnn a) (PadsTyAnn a) Exp
  | PListAnn a (PadsTyAnn a) (Maybe (PadsTyAnn a)) (Maybe (TermCondAnn a))
  | PPartitionAnn a (PadsTyAnn a) Exp
  | PValueAnn a Exp (PadsTyAnn a)
  | PAppAnn a [(PadsTyAnn a)] (Maybe Exp)
  | PTupleAnn a [(PadsTyAnn a)]
  | PExpressionAnn a Exp
  | PTyconAnn a QString
  | PTyvarAnn a String
     deriving (Eq, Data, Typeable, Show)


unPadsTyAnn :: PadsTyAnn a -> (a, PadsTy)
unPadsTyAnn (PConstrainAnn ann x0 x1 x2) = (ann, PConstrain x0 (viewC x1) x2)
unPadsTyAnn (PTransformAnn ann x0 x1 x2) = (ann, PTransform (viewC x0) (viewC x1) x2)
unPadsTyAnn (PListAnn ann x0 x1 x2) = (ann, PList (viewC x0) (viewC <$> x1) (viewC <$> x2))
unPadsTyAnn (PPartitionAnn ann x0 x1) = (ann, PPartition (viewC x0) x1)
unPadsTyAnn (PValueAnn ann x0 x1) = (ann, PValue x0 (viewC x1))
unPadsTyAnn (PAppAnn ann x0 x1) = (ann, PApp (map viewC x0) x1)
unPadsTyAnn (PTupleAnn ann x0) = (ann, PTuple (map viewC x0))
unPadsTyAnn (PExpressionAnn ann x0) = (ann, PExpression x0)
unPadsTyAnn (PTyconAnn ann x0) = (ann, PTycon x0)
unPadsTyAnn (PTyvarAnn ann x0) = (ann, PTyvar x0)

instance Annotation PadsTyAnn where
  getAnn = fst . unPadsTyAnn

instance Annotated PadsTy PadsTyAnn where
  ann = annPadsTy
  unAnn = unPadsTyAnn

annPadsTy :: a -> PadsTy -> PadsTyAnn a
annPadsTy ann (PConstrain x0 x1 x2) =
  PConstrainAnn ann x0 (annPadsTy ann x1) x2
annPadsTy ann (PTransform x0 x1 x2) =
  PTransformAnn ann (annPadsTy ann x0) (annPadsTy ann x1) x2
annPadsTy ann (PList x0 x1 x2) =
  PListAnn ann (annPadsTy ann x0) (annPadsTy ann <$> x1) (annTermCond ann <$> x2)
annPadsTy ann (PPartition x0 x1) =
  PPartitionAnn ann (annPadsTy ann x0) x1
annPadsTy ann (PValue x0 x1) = PValueAnn ann x0 (annPadsTy ann x1)
annPadsTy ann (PApp x0 x1) = PAppAnn ann (map (annPadsTy ann) x0) x1
annPadsTy ann (PTuple x0) = PTupleAnn ann (map (annPadsTy ann) x0)
annPadsTy ann (PExpression x0) = PExpressionAnn ann x0
annPadsTy ann (PTycon x0) = PTyconAnn ann x0
annPadsTy ann (PTyvar x0) = PTyvarAnn ann x0

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

data TermCondAnn a =
    LTermAnn a (PadsTyAnn a)
  | LLenAnn a Exp
     deriving (Eq, Data, Typeable, Show)

unTermCondAnn :: TermCondAnn a -> (a, TermCond)
unTermCondAnn (LTermAnn ann x0) = (ann, LTerm (viewC x0))
unTermCondAnn (LLenAnn ann x0) = (ann, LLen x0)

instance Annotation TermCondAnn where
  getAnn = fst . unTermCondAnn

instance Annotated TermCond TermCondAnn where
  ann = annTermCond
  unAnn = unTermCondAnn

annTermCond :: a -> TermCond -> TermCondAnn a
annTermCond ann (LTerm x0) = LTermAnn ann (annPadsTy ann x0)
annTermCond ann (LLen x0) = LLenAnn ann x0

data TermCond = LTerm PadsTy
              | LLen Exp
  deriving (Eq, Data, Typeable, Show)

data PadsDataAnn a =
    PUnionAnn a [(BranchInfoAnn a)]
  | PSwitchAnn a Exp [(Pat, BranchInfoAnn a)]
     deriving (Eq, Data, Typeable, Show)

unPadsDataAnn :: PadsDataAnn a -> (a, PadsData)
unPadsDataAnn (PUnionAnn ann x0) = (ann, PUnion (map viewC x0))
unPadsDataAnn (PSwitchAnn ann x0 x1) =
  (ann, PSwitch x0 (map (\(x,y) -> (x, viewC y)) x1))

vPadsData :: PadsDataAnn a -> PadsData
vPadsData = snd . unPadsDataAnn

instance Annotation PadsDataAnn where
  getAnn = fst . unPadsDataAnn

instance Annotated PadsData PadsDataAnn where
  ann = annPadsData
  unAnn = unPadsDataAnn

annPadsData :: a -> PadsData -> PadsDataAnn a
annPadsData ann (PUnion x0) = PUnionAnn ann (map (annBranchInfo ann) x0)
annPadsData ann (PSwitch x0 x1)  =
  PSwitchAnn ann x0 (map (\(x,y) -> (x, annBranchInfo ann y)) x1)

data PadsData = PUnion [BranchInfo]
              | PSwitch Exp [(Pat,BranchInfo)]
  deriving (Eq, Data, Typeable, Show)

data BranchInfoAnn a =
    BRecordAnn a String [FieldInfoAnn a] (Maybe Exp)
  | BConstrAnn a String [ConstrArgAnn a] (Maybe Exp)
     deriving (Eq, Data, Typeable, Show)

unBranchInfoAnn :: BranchInfoAnn a -> (a, BranchInfo)
unBranchInfoAnn (BRecordAnn ann x0 x1 x2) =
  (ann, BRecord x0 (map viewC x1) x2)
unBranchInfoAnn (BConstrAnn ann x0 x1 x2) =
  (ann, BConstr x0 (map viewC x1) x2)

instance Annotation BranchInfoAnn where
  getAnn = fst . unBranchInfoAnn

instance Annotated BranchInfo BranchInfoAnn where
  ann = annBranchInfo
  unAnn = unBranchInfoAnn

annBranchInfo :: a -> BranchInfo -> BranchInfoAnn a
annBranchInfo ann (BRecord x0 x1 x2) =
  BRecordAnn ann x0 (map (annFieldInfo ann) x1) x2
annBranchInfo ann (BConstr x0 x1 x2) =
  BConstrAnn ann x0 (map (annConstrArg ann) x1) x2

data BranchInfo = BRecord String [FieldInfo] (Maybe Exp)
                | BConstr String [ConstrArg] (Maybe Exp)
  deriving (Eq, Data, Typeable, Show)

newtype FieldInfoAnn a =
  FieldInfoAnn (a, (Maybe String, ConstrArgAnn a, Maybe Exp))
    deriving (Eq, Data, Typeable, Show)

unFieldInfoAnn :: FieldInfoAnn a -> (a, FieldInfo)
unFieldInfoAnn (FieldInfoAnn (y, (x0, x1, x2))) = (y, (x0, viewC x1, x2))

instance Annotation FieldInfoAnn where
  getAnn = fst . unFieldInfoAnn

instance Annotated FieldInfo FieldInfoAnn where
  ann = annFieldInfo
  unAnn = unFieldInfoAnn

annFieldInfo :: a -> FieldInfo -> FieldInfoAnn a
annFieldInfo ann (x0, x1, x2) =
  FieldInfoAnn (ann, (x0, annConstrArg ann x1, x2))

type FieldInfo = (Maybe String, ConstrArg, Maybe Exp)



newtype ConstrArgAnn a = ConstrArgAnn (a, (Strict, PadsTyAnn a))
    deriving (Eq, Data, Typeable, Show)

unConstrArgAnn :: ConstrArgAnn a -> (a, ConstrArg)
unConstrArgAnn (ConstrArgAnn (y, (x0, x1))) = (y, (x0, viewC x1))

instance Annotation ConstrArgAnn where
  getAnn = fst . unConstrArgAnn

instance Annotated ConstrArg ConstrArgAnn where
  ann = annConstrArg
  unAnn = unConstrArgAnn

annConstrArg :: a -> ConstrArg -> ConstrArgAnn a
annConstrArg ann (x0, x1) = ConstrArgAnn (ann, (x0, annPadsTy ann x1))

type ConstrArg = (Strict, PadsTy)

type QString = [String]  -- qualified names


hasRep :: PadsTy -> Bool
hasRep (PExpression l)   = False
hasRep (PTycon ["EOF"])  = False
hasRep (PTycon ["EOR"])  = False
hasRep (PTycon ["Void"]) = False
hasRep ty                 = True

qName :: QString -> String
qName [n] = n
qName (n:ms) = n ++ "." ++ qName ms



