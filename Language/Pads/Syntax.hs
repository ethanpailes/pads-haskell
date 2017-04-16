{-# LANGUAGE DeriveDataTypeable, MultiParamTypeClasses,
    TypeSynonymInstances, FlexibleInstances, TemplateHaskell,
    FlexibleContexts, ViewPatterns #-}

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
import Control.Monad (liftM, ap)
import qualified Control.Monad.Trans as CMT
import Control.Monad.State.Lazy (MonadState(..), StateT)
import Language.Pads.Generic


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


data ForceM a = ForceM a
              | DeferM a -- ^ this is just so that monadic bind behaves the way we want

newtype PadsParseState s a = PadsParseState {
      runPadsState :: (s -> (ForceM a, s))
    }

instance Functor (PadsParseState s) where
    fmap = liftM

instance Applicative (PadsParseState s) where
    pure x = PadsParseState $ \s -> (ForceM x, s)
    (<*>) = ap

instance Monad (PadsParseState s) where
  PadsParseState runSt1 >>= f =
    PadsParseState $ \s1 ->
      let (x1, s2) = runSt1 s1
      in case x1 of
           (ForceM v) -> runPadsState (f v) s2
           -- we still invoke the function so that the state actions
           -- get run. the funciton might force the thunk, which would be sad.
           (DeferM x) -> runPadsState (f x) s2

-- | thread state through, even if we have been asked to defer
threadState :: PadsParseState s a -> PadsParseState s b -> PadsParseState s b
threadState (PadsParseState runSt1) (PadsParseState runSt2) =
  PadsParseState $ \s1 ->
   let (_, s2) = runSt1 s1
       (x, s3) = runSt2 s2
    in (x, s3)

defer' :: PadsDefault () a md => PadsParseState s a
defer' = PadsParseState $ \s -> (DeferM (def1 ()), s)

force' :: a -> PadsParseState s a
force' = return

-- instance MonadState PadsParseState s where
state f = PadsParseState $ \s -> let (x, s') = f s in (ForceM x, s')
get = PadsParseState $ \s -> (ForceM s, s)
put newSt = PadsParseState $ \_ -> (ForceM (), newSt)

-- do { res <- parseRes
--    ; let ps = forceFun res
--    }
--

-- the parse functions can be of type
-- st -> PadsParser (a, a_md, st)
-- where a is the type to be parsed, st is the parse state type
-- and a_md is the metadata type for the parse.
-- then I need to add some state into the PadsParser monad itself.

data Keep a = Keep a
            | Discard

-- | A pads skin tells PADS which values to force during
--   parsing. One can be declared like
--   `skin SkinName for SomePadsType = <pattern>`
--   or just with
--   `skin SkinName = <pattern>`
--   if you don't want to apply the skin to a type just yet
data PadsSkinPat = PSBind Exp -- ^ bind a function of type `a s -> (Force a, s)`
                              -- to the match cite, where a is the type that gets
                              -- parsed and s is the fold state.

                 -- TODO: delete
                 | PSForce -- ^ Force the type. Terminal.
                 | PSDefer -- ^ Defer the type for later parsing. Terminal.

                 -- pattern stuff
                 | PSConP QString [PadsSkinPat] -- ^ constructor patterns
                 | PSRecP QString [(String, PadsSkinPat)] -- ^ record patterns
                 | PSTupleP [PadsSkinPat] -- ^ tuple patterns

                 -- | A reference to a previously defined skin
                 | PSSkin QString
   deriving (Eq, Data, Typeable, Show)

-- | the expression to force a result
force :: Exp
force = LamE [VarP x, VarP s] $ (TupE [AppE (ConE 'Keep) (VarE x),VarE s])
  where x = mkName "x"
        s = mkName "s"

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


-- | replace a type variable with a given type
replaceTyVar :: String -- ^ the ty var to replace
             -> PadsTy -- ^ the type to replace it with
             -> PadsTy -- ^ the source type
             -> PadsTy
replaceTyVar a ty' (PTyvar x) | a == x = ty'
replaceTyVar _ _ (PTyvar x) = (PTyvar x)
replaceTyVar a ty' (PConstrain pat (replaceTyVar a ty' -> ty) exp) =
  PConstrain pat ty exp
replaceTyVar a ty' (PTransform (replaceTyVar a ty' -> ty1)
                               (replaceTyVar a ty' -> ty2) exp) =
  PTransform ty1 ty2 exp
replaceTyVar a ty' (PList (replaceTyVar a ty' -> ty1)
                          (liftM (replaceTyVar a ty') -> ty2) termCond) =
  PList ty1 ty2 termCond
replaceTyVar a ty' (PPartition (replaceTyVar a ty' -> ty) exp) =
  PPartition ty exp
replaceTyVar a ty' (PValue exp (replaceTyVar a ty' -> ty)) =
  PValue exp ty
replaceTyVar a ty' (PApp (map (replaceTyVar a ty') -> tys) exp) =
  PApp tys exp
replaceTyVar a ty' (PTuple (map (replaceTyVar a ty') -> tys)) =
  PTuple tys
replaceTyVar a ty' (PExpression exp) = PExpression exp
replaceTyVar a ty' (PTycon con) = PTycon con


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

