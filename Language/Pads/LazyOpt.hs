{-# LANGUAGE ViewPatterns, TemplateHaskell #-}


module Language.Pads.LazyOpt where

import Text.Regex.Posix ((=~))
import Debug.Trace
import Control.Monad
import Data.Maybe (fromMaybe)
import Language.Pads.Syntax
import Data.List (intersperse)

import Language.Haskell.TH
import Language.Haskell.TH.Syntax as THS
import Language.Haskell.TH.Lift (deriveLift)
import Data.Data
import Data.IORef
import System.IO.Unsafe

import qualified Data.Map.Strict as Map


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

-- | returns an expression of type (PadsTy, SkipStrategy)
ssPadsTy :: PadsTy -> Q (PadsTy, SkipStrategy)
ssPadsTy ty@(PTycon [fixedPrim -> Just n]) = return (ty, (SSFixed n))
ssPadsTy ty@(PTycon tc) = do
  ssEnv <- pcgGetSSEnv
  case tc `Map.lookup` ssEnv of
    (Just (_, ss)) -> return (ty, ss)
    Nothing -> do
      ty' <- pcgGetTy tc
      ssPadsTy ty'

{- this needs to handle the expression arg
ssPadsTy ty@(PApp (PTycon tc : args) _) = do
  let ss = [| do { let ss' = pcg_METADATA_skipStrategy
                             $(return . VarE . mkName $
                                 padsCGMetadataPrefix ++ qName tc)
              ;  argSkips <- argSkipStrats args
              ;  applySSFun ss' (ListE argSkips)
              }
        |]
  [| (ty, $ss) |]
  -}


ssPadsTy ty = return $ ssPadsTy' ty -- lets be typesafe when we can


-- | returns a list of expressions of type SkipStrategy
argSkipStrats :: [PadsTy] -> Q [Exp]
argSkipStrats [] = return []
argSkipStrats [ty] = do
  exp <- ssPadsTy ty
  ss <- [| let (_, ss') = exp in ss' |]
  return [ss]
argSkipStrats (ty:ts) = do
  rest <- argSkipStrats ts
  exp <- ssPadsTy ty
  ss <- [| let (_, ss') = exp in ss' |]
  return (ss:rest)

ssPadsTy' :: PadsTy -> (PadsTy, SkipStrategy)

-- primitive fixed width types
ssPadsTy' ty@(PTycon tc@[fixedPrim -> Just n]) = (ty, (SSFixed n))

-- fixed width strings
ssPadsTy' ty@(PApp tc@[PTycon ["StringFW"]] e@(Just (evalIntExp -> Just n))) =
  (ty, (SSFixed n))

-- fixed hex numbers TODO: do we really need this once recursion is working?
ssPadsTy' ty@(PApp tc@[PTycon ["Phex32FW"]] e@(Just (evalIntExp -> Just n))) =
  (ty, (SSFixed n))

-- literals
ssPadsTy' ty@(PExpression e@(fixedLit -> Just n)) =
  (ty, (SSFixed n))

-- fixed width lists, with fixed width elements
ssPadsTy' ty@(PList (ssPadsTy' -> memberTy)
                  ((ssPadsTy' <$>) -> sepTy)
                  l@(Just (LLen (evalIntExp -> Just len)))) =
      (ty, SSSeq (case sepTy of
                    (Just sep) -> intersperse sep $ replicate len memberTy
                    Nothing -> replicate len memberTy))

-- Tuples
ssPadsTy' ty@(PTuple (map ssPadsTy' -> taus)) = (ty, SSSeq taus)


-- Cases that I'm not so sure about.
-- For some of these I could not convince the parser to emit good values
-- so, they are untested
ssPadsTy' ty@(PTransform (ssPadsTy' -> (_, tau1)) _ _) =
  (ty, tau1)
ssPadsTy' ty@(PPartition (ssPadsTy' -> tau) e) = -- TODO: this needs to do something with record discipline
  (ty, snd tau)
ssPadsTy' ty@(PValue e (ssPadsTy' -> tau)) = (ty, SSFixed 0)

-- ssPadsTy' ty@(PApp [PTycon tc] _) =
--   THS.lift (ty, VarE (mkName $ padsCGMetadataPrefix ++ qName tc) )

-- ssPadsTy' ty@(PApp ((PTycon tc):args) _) =
--   (ty, [| $(padsCGMetadataPrefix ++ tc) |])

-- default
ssPadsTy' t = (t, SSNone)

-- Pads Datatypes
--

-- | returns an expression of type (ConstrArg, SkipStrategy)
ssConstArg :: ConstrArg -> Q (ConstrArg, SkipStrategy)
ssConstArg ca@(_, ty) = do
  (_, t) <- ssPadsTy ty
  return (ca, t)

-- | returns an expression of type (PadsData, SkipStrategy)
ssPadsData :: PadsData -> Q (PadsData, SkipStrategy)

-- unions are fixed width iff all branches are fixed width
ssPadsData u@(PUnion branches) = do
  bs <- branchSizes
  return $ case bs of
              (Just (b:bs)) -> if all (==b) bs
                                then (u, SSFixed b)
                                else (u, SSNone)
              (Just []) -> (u, SSFixed 0)
              Nothing -> (u, SSNone)
  where branchSizes :: Q (Maybe [Int])
        branchSizes = sequence <$> mapM branchSize branches
        branchSize :: BranchInfo -> Q (Maybe Int)
        branchSize (BRecord _ fieldInfo exp) = do
            -- fieldSizes : Q [Maybe Int]
            fieldSizes <-
                mapM (\(_, (_, ty), _) -> do
                         (_, t) <- optimise <$> ssPadsTy ty
                         return (case t of
                                  (SSFixed n) -> Just n
                                  _ -> Nothing))
                fieldInfo
            return $ foldr (liftM2 (+)) (Just 0) fieldSizes
  -- TODO(ethan): BConstr branch
        branchSize _ = return Nothing

ssPadsData d = return (d, SSNone)


-- Pads Declaration
--

ssPadsDecl :: PadsDecl -> Q (PadsDecl, SkipStrategy)
-- type aliases
ssPadsDecl ptd@(PadsDeclType n as p tau) = do
  (_, t) <- ssPadsTy tau
  return (ptd, t)

ssPadsDecl t = return (t, SSNone)


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
-- to actually eval stuff
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



----------------------------------------------------------------------
-- environment hack
----------------------------------------------------------------------

-- piggy-back the lookup environment on haskell's value environement
-- TODO: delete
{-
data PadsCodeGenMetadata = PadsCodeGenMetadata {
        pcg_METADATA_skipStrategy :: (PadsDecl, SkipStrategy)
    }
    | PadsCodeGenSkinMetadata {
        pcg_METADATA_skin :: PadsSkinPat
    }
  deriving(Eq, Show)

padsCGMetadataPrefix :: String
padsCGMetadataPrefix = "pads_CG_METADATA_"
-}

type Env a = Map.Map QString a

data PadsCGM = PadsCGM {
      pcg_skipStrategyEnv :: Env (PadsDecl, SkipStrategy)
    , pcg_typeEnv :: Env PadsTy
    , pcg_skinEnv :: Env PadsSkinPat
    }
  deriving(Eq, Show)


{-# NOINLINE padsCodeGenEnv #-}
padsCodeGenEnv :: IORef PadsCGM
padsCodeGenEnv =
  unsafePerformIO $ newIORef PadsCGM {
    pcg_skipStrategyEnv =
       Map.fromList [
        (["Int"], (PadsDeclType "Int" [] Nothing (PTycon ["Int"]), SSNone))
       ]
  , pcg_typeEnv =
       Map.fromList [
        (["Int"], PTycon ["Int"])
      ]
  , pcg_skinEnv = Map.empty
  }

-- | Add a skip strategy annotation to the pads codegen environment
pcgPutSS :: QString -> (PadsDecl, SkipStrategy) -> Q ()
pcgPutSS name ss =
  qRunIO $ modifyIORef padsCodeGenEnv
     (\pcgm -> pcgm {
         pcg_skipStrategyEnv = Map.insert name ss (pcg_skipStrategyEnv pcgm)
     })

-- | Looks up a given skip strategy in the pads codegen environment.
--     Makes use of the MonadFail instance if one is not defined.
pcgGetSS :: QString -> Q (PadsDecl, SkipStrategy)
pcgGetSS name = do
  env <- qRunIO $ readIORef padsCodeGenEnv
  case name `Map.lookup` (pcg_skipStrategyEnv env) of
    (Just ss) -> return ss
    Nothing -> fail $ "SkipStrategy for Type: "
                           ++ qName name ++ " is not defined!"

-- | Add a skip strategy annotation to the pads codegen environment
pcgPutTy :: QString -> PadsTy -> Q ()
pcgPutTy name ss =
  qRunIO $ modifyIORef padsCodeGenEnv
     (\pcgm -> pcgm {
         pcg_typeEnv = Map.insert name ss (pcg_typeEnv pcgm)
     })

-- | Looks up a given skip strategy in the pads codegen environment.
--     Makes use of the MonadFail instance if one is not defined.
pcgGetTy :: QString -> Q PadsTy
pcgGetTy name = do
  env <- qRunIO $ readIORef padsCodeGenEnv
  case name `Map.lookup` (pcg_typeEnv env) of
    (Just ss) -> return ss
    Nothing -> fail $ "Type: " ++ qName name ++ " is not defined!"

-- | Add a annotation to the pads codegen environment
pcgPutSkin :: QString -> PadsSkinPat -> Q ()
pcgPutSkin name ss =
  qRunIO $ modifyIORef padsCodeGenEnv
     (\pcgm -> pcgm {
         pcg_skinEnv = Map.insert name ss (pcg_skinEnv pcgm)
     })

pcgGetTySS :: QString -> Q (PadsTy, SkipStrategy)
pcgGetTySS name = do
  env <- qRunIO $ readIORef padsCodeGenEnv
  case name `Map.lookup` (pcg_skipStrategyEnv env) of
    (Just (PadsDeclType _ _ _ ty, ss)) -> return (ty, ss)
    _ ->
      fail $ "Can't find skip strategy information for type:\n"
             ++ show (qName name)
             ++ "\nThis is likely a result of a bug in PADS."

-- | Looks up a given skin in the pads codegen environment.
--     Makes use of the MonadFail instance if one is not defined.
pcgGetSkin :: QString -> Q PadsSkinPat
pcgGetSkin name = do
  env <- qRunIO $ readIORef padsCodeGenEnv
  case name `Map.lookup` (pcg_skinEnv env) of
    (Just s) -> return s
    Nothing -> fail $ "Skin: " ++ qName name ++ " is not defined!"

-- | get the skip strat env
pcgGetSSEnv :: Q (Env (PadsDecl, SkipStrategy))
pcgGetSSEnv = do
  env <- qRunIO $ readIORef padsCodeGenEnv
  return $ pcg_skipStrategyEnv env

-- | get the skin env
pcgGetSkinEnv :: Q (Env PadsSkinPat)
pcgGetSkinEnv = do
  env <- qRunIO $ readIORef padsCodeGenEnv
  return $ pcg_skinEnv env

-- | get the ty env
pcgGetTyEnv :: Q (Env PadsTy)
pcgGetTyEnv = do
  env <- qRunIO $ readIORef padsCodeGenEnv
  return $ pcg_typeEnv env

----------------------------------------------------------------------
-- TH goes at the end of the file to avoid breaking mutual recursion
----------------------------------------------------------------------

$(deriveLift ''SkipStrategy)
