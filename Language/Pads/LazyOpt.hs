{-# LANGUAGE ViewPatterns, TemplateHaskell #-}


module Language.Pads.LazyOpt where

import Text.Regex.Posix ((=~))
import Debug.Trace
import Control.Monad
import Data.Maybe (fromMaybe)
import Language.Pads.Syntax
import Data.List (intersperse)

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Lift (deriveLift)

----------------------------------------------------------------------
-- Analysis Step
----------------------------------------------------------------------

-- | An AST annotation to indicate the best way to skip over the given
--   type without actually parsing it for information.
data SkipStrategy =
    SSFixed Int
  | SSSeq [(PadsTy, SkipStrategy)]
  -- SSFun's args are spiritually of type ([SkipStrategy] -> SkipStrategy)
  -- the expression expects to be evaluated in a context where a value
  -- of type SkipStrategy is bound to each of the Name's in the argument
  -- list. This hackery is required because otherwise there is no way
  -- to provide a Lift instance for SkipStrategy, which is required if
  -- we want to stuff it in the generated PADS_CG_METADATA fields.
  | SSFun [Name] Exp
  | SSNone
    deriving(Eq, Show)

applySSFun :: SkipStrategy -> Exp -> Q Exp
applySSFun (SSFun ns body) (ListE es) =
  return $ LetE [ValD (VarP n) (NormalB e) [] | (n, e) <- zip ns es] body
applySSFun _ _ =
  fail "applySSFun: called on non-SSFun arg, or with non-list expression"


-- Pads Types
--

ssPadsTy :: PadsTy -> (PadsTy, SkipStrategy)

-- primitive fixed width types
ssPadsTy ty@(PTycon tc@[fixedPrim -> Just n]) = (ty, (SSFixed n))

-- fixed width strings
ssPadsTy ty@(PApp tc@[PTycon ["StringFW"]] e@(Just (evalIntExp -> Just n))) =
  (ty, (SSFixed n))

-- fixed hex numbers TODO: do we really need this once recursion is working?
ssPadsTy ty@(PApp tc@[PTycon ["Phex32FW"]] e@(Just (evalIntExp -> Just n))) =
  (ty, (SSFixed n))

-- literals
ssPadsTy ty@(PExpression e@(fixedLit -> Just n)) =
  (ty, (SSFixed n))

-- fixed width lists, with fixed width elements
ssPadsTy ty@(PList (ssPadsTy -> memberTy)
                  ((ssPadsTy <$>) -> sepTy)
                  l@(Just (LLen (evalIntExp -> Just len)))) =
      (ty, SSSeq (case sepTy of
                    (Just sep) -> intersperse sep $ replicate len memberTy
                    Nothing -> replicate len memberTy))

-- Tuples
ssPadsTy ty@(PTuple (map ssPadsTy -> taus)) = (ty, SSSeq taus)


-- Cases that I'm not so sure about.
-- For some of these I could not convince the parser to emit good values
-- so, they are untested
ssPadsTy ty@(PTransform (ssPadsTy -> tau1) (ssPadsTy -> tau2) e) =
  (ty, snd tau1)
ssPadsTy ty@(PPartition (ssPadsTy -> tau) e) = -- TODO: this needs to do something with record discipline
  (ty, snd tau)
ssPadsTy ty@(PValue e (ssPadsTy -> tau)) = (ty, SSFixed 0)

-- default
ssPadsTy t = (t, SSNone)

-- Pads Datatypes
--

ssConstArg :: ConstrArg -> (ConstrArg, SkipStrategy)
ssConstArg ca@(_, (ssPadsTy -> ty)) = (ca, snd ty)


ssPadsData :: PadsData -> (PadsData, SkipStrategy)

-- unions are fixed width iff all branches are fixed width
ssPadsData u@(PUnion branches) =
  case sequence branchSizes of
      (Just (b:bs)) -> if all (==b) bs
                         then (u, SSFixed b)
                         else (u, SSNone)
      (Just []) -> (u, SSFixed 0)
      Nothing -> (u, SSNone)
  where branchSizes = map branchSize branches
        branchSize :: BranchInfo -> Maybe Int
        branchSize (BRecord _ fieldInfo exp) =
          foldr (liftM2 (+)) (Just 0)
            (map (\(_, (_, ssPadsTy -> ty), _) ->
                    case snd ty of
                      (SSFixed n) -> Just n
                      SSNone -> Nothing) fieldInfo)
  -- TODO(ethan): BConstr branch
        branchSize _ = Nothing

ssPadsData d = (d, SSNone)


-- Pads Declaration
--

ssPadsDecl :: PadsDecl -> (PadsDecl, SkipStrategy)
-- type aliases
ssPadsDecl ptd@(PadsDeclType n as p (ssPadsTy -> tau)) =
  (ptd, snd tau)

ssPadsDecl t = (t, SSNone)


-- | Optimise a skip strategy
optimise :: (PadsTy, SkipStrategy) -> (PadsTy, SkipStrategy)
optimise (ty, (SSSeq [])) = (ty, SSFixed 0)
optimise (ty, (SSSeq [(_, SSFixed n)])) = (ty, SSFixed n)
optimise (ty, (SSSeq ((_, SSFixed 0):ss))) =
  optimise (ty, (SSSeq ss))
optimise (ty, (SSSeq ((ty', SSFixed n):(_, SSFixed m):rest))) =
  case optimise (PTyvar "BOGUS", (SSSeq rest)) of
    (_, (SSSeq ss)) -> (ty, SSSeq ((ty', SSFixed (n + n)):ss))
    (_, (SSFixed k)) -> (ty, SSFixed (n + m + k))
    ss -> (ty, SSSeq [(ty', SSFixed (n + m)), ss])

-- fuse lists. We can drop the inner list type, because the elements
-- are the only things that take up actual space on disk.
optimise (ty, (SSSeq ((_, SSSeq ss1):ss2))) =
  optimise (ty, SSSeq $ ss1 ++ ss2)

optimise (ty, (SSSeq (s:ss))) =
  case optimise (PTyvar "BOGUS", (SSSeq ss)) of
    (_, SSSeq ss') -> (ty, SSSeq (s:ss'))
    ss' -> (ty, SSSeq [s, ss'])
optimise ss = ss

-- for now, just handles the literal case, but eventually it would be nice
-- to actually parse stuff and eval it
evalIntExp :: Exp -> Maybe Int
evalIntExp (LitE (IntegerL n)) = Just . fromIntegral $ n
evalIntExp _ = Nothing

isFixedWidth :: (a, SkipStrategy) -> Bool
isFixedWidth (_, (SSFixed _)) = True
isFixedWidth _ = False

fixedPrim :: String -> Maybe Int
fixedPrim "Char" = Just 1
fixedPrim "Digit" = Just 1
fixedPrim _ = Nothing

fixedLit :: Exp -> Maybe Int
fixedLit (LitE (CharL _)) = Just 1
fixedLit (AppE (ConE con) (LitE (StringL s))) | nameBase con == "RE" =
    if (s =~ "[ a-zA-Z0-9]*" :: String) == s
        then Just . length $ s
    else Nothing
fixedLit _ = Nothing

$(deriveLift ''SkipStrategy)
