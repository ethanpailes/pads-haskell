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
import Control.Monad (liftM, ap)
import qualified Control.Monad.Trans as CMT
import Control.Monad.State.Lazy (MonadState(..), StateT)

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


newtype PadsParseState s a = PadsParseState {
      runPadsState :: (s -> (Maybe a, s))
    }

instance Functor (PadsParseState s) where
    fmap = liftM

instance Applicative (PadsParseState s) where
    pure x = PadsParseState $ \s -> (Just x, s)
    (<*>) = ap

instance Monad (PadsParseState s) where
  PadsParseState runSt1 >>= f =
    PadsParseState $ \s1 ->
      let (x1, s2) = runSt1 s1
      in case x1 of
           (Just v) -> runPadsState (f v) s2
           -- note that no state actions are run once we start defering
           Nothing -> (Nothing, s2)

-- | thread state through, even if we have been asked to defer
threadState :: PadsParseState s a -> PadsParseState s b -> PadsParseState s b
threadState (PadsParseState runSt1) (PadsParseState runSt2) =
  PadsParseState $ \s1 ->
   let (_, s2) = runSt1 s1
       (x, s3) = runSt2 s2
    in (x, s3)

defer :: PadsParseState s a
defer = PadsParseState $ \s -> (Nothing, s)

force :: a -> PadsParseState s a
force = return

-- instance MonadState PadsParseState s where
state f = PadsParseState $ \s -> let (x, s') = f s in (Just x, s')
get = PadsParseState $ \s -> (Just s, s)
put newSt = PadsParseState $ \_ -> (Just (), newSt)

-- do { res <- parseRes
--    ; let ps = forceFun res
--    }
--

-- the parse functions can be of type
-- st -> PadsParser (a, a_md, st)
-- where a is the type to be parsed, st is the parse state type
-- and a_md is the metadata type for the parse.
-- then I need to add some state into the PadsParser monad itself.


-- | A pads skin tells PADS which values to force during
--   parsing. One can be declared like
--   `skin SkinName for SomePadsType = <pattern>`
--   or just with
--   `skin SkinName = <pattern>`
--   if you don't want to apply the skin to a type just yet
data PadsSkinPat = PSBind Exp -- ^ bind a function of type `a -> m a` to
                              --   the match cite, where a is the type that gets
                              --   parsed and m is the monad.

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

