{-# LANGUAGE ViewPatterns, TemplateHaskell #-}


module Language.Pads.LazyOpt where

import Text.Regex.Posix ((=~))
import Debug.Trace
import Control.Monad
import Data.Maybe (fromMaybe)
import Language.Haskell.TH (Exp(..), Lit(..), nameBase)
import Language.Haskell.TH.Syntax (Strict(..))
import Language.Pads.Syntax
import Language.Pads.RegExp

----------------------------------------------------------------------
-- Analysis Step
----------------------------------------------------------------------

-- | An AST annotation to indicate the best way to skip over the given
--   type without actually parsing it for information.
data SkipStrategy = SSFixed Int
                  | SSSeq [SkipStrategy]
                  | SSFun ([SkipStrategy] -> SkipStrategy)
                  | SSNone

instance Show SkipStrategy where
  show (SSFixed i) = "(SSFixed " ++ show i ++ ")"
  show (SSSeq ss) = "(SSSeq " ++ show ss ++ ")"
  show (SSFun _) = "(SSFun [...])"
  show SSNone = "SSNone"

-- Not true equality, but still useful
instance Eq SkipStrategy where
  SSFixed n1 == SSFixed n2 = n1 == n2
  SSSeq s1 == SSSeq s2 = s1 == s2
  SSNone == SSNone = True
  _ == _ = False

-- Pads Types
--

ssPadsTy :: PadsTy -> PadsTyAnn SkipStrategy

-- primitive fixed width types
ssPadsTy (PTycon tc@[fixedPrim -> Just n]) = PTyconAnn (SSFixed n) tc

-- fixed width strings
ssPadsTy (PApp tc@[PTycon ["StringFW"]] e@(Just (evalIntExp -> Just n))) =
  PAppAnn (SSFixed n) (map (annPadsTy SSNone) tc) e

-- literals
ssPadsTy (PExpression e@(fixedLit -> Just n)) =
  PExpressionAnn (SSFixed n) e

-- fixed width lists
ssPadsTy (PList (ssPadsTy -> memberTy)
                  ((ssPadsTy <$>) -> sepTy)
                  l@(Just (LLen (evalIntExp -> Just len)))) =
  PListAnn (maybe SSNone SSFixed byteLength)
    memberTy sepTy (ann SSNone <$> l)
  where byteLength = do
          let (SSFixed n) = getAnn memberTy
          let (SSFixed m) = fromMaybe (SSFixed 0) $ getAnn <$> sepTy
          Just $ (len * n) + ((len - 1) * m)

-- Tuples
ssPadsTy (PTuple (map ssPadsTy -> taus)) =
  let skipStrats = map getAnn taus
      width = foldr (\ss acc -> case ss of
                                  (SSFixed n) -> (liftM2 (+)) acc (return n)
                                  _ -> Nothing)
                    (Just 0)
                    skipStrats
   in PTupleAnn
        (maybe (if (all (==SSNone) skipStrats) then SSNone else SSSeq skipStrats)
                  SSFixed width)
        taus


-- Cases that I'm not so sure about.
-- For some of these I could not convince the parser to emit good values
-- so, they are untested
ssPadsTy (PTransform (ssPadsTy -> tau1) (ssPadsTy -> tau2) e) =
  PTransformAnn (getAnn tau1) tau1 tau2 e
ssPadsTy (PPartition (ssPadsTy -> tau) e) =
  PPartitionAnn (getAnn tau) tau e
ssPadsTy (PValue e (ssPadsTy -> tau)) = PValueAnn (SSFixed 0) e tau

-- default
ssPadsTy t = annPadsTy SSNone t

-- Pads Datatypes
--

ssConstArg :: ConstrArg -> ConstrArgAnn SkipStrategy
ssConstArg ca@(x, (ssPadsTy -> ty)) = annConstrArg (getAnn ty) ca

ssPadsData :: PadsData -> PadsDataAnn SkipStrategy

-- unions are fixed width iff all branches are fixed width
ssPadsData u@(PUnion branches) =
  case sequence branchSizes of
      (Just (b:bs)) -> if all (==b) bs
                         then annPadsData (SSFixed b) u
                         else annPadsData SSNone u
      (Just []) -> annPadsData (SSFixed 0) u
      Nothing -> annPadsData SSNone u
  where branchSizes = map branchSize branches
        branchSize :: BranchInfo -> Maybe Int
        branchSize (BRecord _ fieldInfo exp) =
          foldr (liftM2 (+)) (Just 0)
            (map (\(_, (_, ssPadsTy -> ty), _) ->
                    case getAnn ty of
                      (SSFixed n) -> Just n
                      SSNone -> Nothing) fieldInfo)
  -- TODO(ethan): BConstr branch
        branchSize _ = Nothing

ssPadsData d = annPadsData SSNone d


-- Pads Declaration
--

ssPadsDecl :: PadsDecl -> PadsDeclAnn SkipStrategy
-- type aliases
ssPadsDecl ptd@(PadsDeclType _ _ _ (getAnn . ssPadsTy -> SSFixed n)) =
  annPadsDecl (SSFixed n) ptd

ssPadsDecl t = annPadsDecl SSNone t

{-
data PadsDecl = PadsDeclType   String [String] (Maybe Pat) PadsTy
              | PadsDeclData   String [String] (Maybe Pat) PadsData [QString]
              | PadsDeclNew    String [String] (Maybe Pat) BranchInfo [QString]
              | PadsDeclObtain String [String] PadsTy Exp
-}



--
-- Helper Functions
--

-- for now, just handles the literal case, but eventually it would be nice
-- to actually parse stuff and eval it
evalIntExp :: Exp -> Maybe Int
evalIntExp (LitE (IntegerL n)) = Just . fromIntegral $ n
evalIntExp _ = Nothing

isFixedWidth :: Annotation a => a SkipStrategy -> Bool
isFixedWidth (getAnn -> (SSFixed _)) = True
isFixedWidth _ = False

fixedPrim :: String -> Maybe Int
fixedPrim "Char" = Just 1
fixedPrim "Digit" = Just 1
fixedPrim _ = Nothing

fixedLit :: Exp -> Maybe Int
fixedLit (LitE (CharL _)) = Just 1
fixedLit (AppE (ConE con) (LitE (StringL s))) | nameBase con == "RE" =
    if s =~ "[ a-zA-Z0-9]*" then Just . length $ s else Nothing
fixedLit _ = Nothing
