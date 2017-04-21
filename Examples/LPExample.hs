{-# LANGUAGE TypeFamilies, TemplateHaskell, QuasiQuotes, MultiParamTypeClasses, FlexibleInstances, DeriveDataTypeable, ScopedTypeVariables, TypeSynonymInstances #-}

module Examples.LPExample where

import Language.Pads.Padsc
import Language.Pads.Generic

import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen
import Test.QuickCheck hiding (Discard)
import Text.PrettyPrint.Mainland as PP
import System.IO (withFile, IOMode(WriteMode, ReadMode), hPutStrLn)
import System.Random (newStdGen, randomR)
import Data.ByteString (hGetContents)
import Control.Applicative (liftA2)
import qualified Language.Pads.Source as S

import Language.Pads.PadsParser (PadsParser, takeHeadP, parseTry)
import Language.Pads.MetaData (cleanBasePD)
import Language.Pads.Syntax

[pads|

newtype Log = Log ([LogEntry | EOR] terminator EOF)

newtype SimpleLog = SimpleLog ([SimpleLogEntry | EOR] terminator EOF)
type SimpleLogEntry = (StringFW 20, Int)

data LogEntry =
  Info { "[ INFO ]"
  , time :: (NextDashEntry Int)
  , subsystemInfo :: SubsystemInfo
  }
| Warning { "[ WARN ]"
  , time :: (NextDashEntry Int)
  , subsystemWarning :: SubsystemWarning
  }
| Error { "[ ERROR ]"
  , time :: (NextDashEntry Int)
  , subsystemError :: SubsystemError
  }

--Presentation For Nate | the payloads for each of the major log categories
data SubsystemWarning =
    SSWarnTransport " - transport" (NextDashEntry TransportWarning)
  | SSWarnRender " - render" (NextDashEntry RenderWarning)
data SubsystemError =
    SSErrorTransport " - transport" (NextDashEntry TransportError)
  | SSErrorRender " - render" (NextDashEntry RenderError)
data SubsystemInfo =
    SSInfoTransport " - transport" (NextDashEntry TransportInfo)
  | SSInfoRender " - render" (NextDashEntry RenderInfo)

type Len = (Int, ":")

--
-- Log entries for the transport subsystem
--
data TransportWarning =
  TSWarnUnstructured { ":unstructured:"
  , tsUWLen :: Len
  , tsUWMessage :: StringFW tsUWLen
  }
  | TSWarnConnectionFailed { ":connect-failed:"
  , tsCFLen :: Len
  , tsCFHost :: StringC ':'
  , ":"
  , tsCFPort :: Int
  }
  | TSWarnRealTimeTimeout { ":realtime-timeout:"
  , tsRTTLen :: Len
  , tsRTTComputeBlock :: StringC ':', ":"
  , tsRTTReason :: Maybe (StringC ':', ":")
  }

data TransportError =
  TSErrorUnstructured { ":unstructured:"
  , tsUELen :: Len
  , tsUEMessage :: StringFW tsUELen
  }

data TransportInfo =
  TSInfoUnstructured { ":unstructured:"
  , tsUILen :: Len
  , tsUIMessage :: StringFW tsUILen
  }
  | TSInfoPacketReceived { ":packet-received:"
  , tsPRLen :: Len
  , tsPRBytes :: (" got ", Int, " bytes")
  , tsPRHost :: (" from host=", StringC ':', ":")
  , tsPRPort :: Int
  }

--
-- Log entries for the render subsystem
--
data RenderWarning =
    RDWarnUnstructured { ":unstructured:"
  , rdUWLen :: Len
  , rdUWMessage :: StringFW rdUWLen
  }
  | RDWarnBoxConflict { ":box-conflict:"
  , rdBCLen :: Len
  , " box1=", rdBCBox1 :: Box
  , " conflicts with "
  , "box2=", rdBCBox2 :: Box
  }

data Box = Box {
  boxUL :: (Point, "-")
, boxUR :: (Point, "-")
, boxLR :: (Point, "-")
, boxLL :: Point
}
type Point = ('(', Int, ',', Int, ')')

data RenderError =
    RDErrorUnstructured { ":unstructured:"
  , rdUELen :: Len
  , rdUEMessage :: StringFW rdUELen
  }
  | RDErrorScreenNotFound { ":screen-not-found:"
  , rdSNFLen :: Len
  , rdSNFErrNo :: (" errno=", Int)
  , rdSNFMessage :: (" message=", StringC '\n')
  }

data RenderInfo =
    RDInfoUnstructured { ":unstructured:"
  , rdUILen :: Len
  , rdUIMessage :: StringFW rdUILen
  }

type NextDashEntry a = (" - ", a)
|]


{- UC1: Skin to get all the errors
 -
 - skin ForceError =
 -   defer case Error force force
 -
 - -- or without the sugar
 - skin ForceError' =
 -   case Error force force
 -      | Info defer defer
 -      | Warning defer defer
 -
 - skin Map s =
 -    case Log (s : Map s)
 -       | Log []
 -
 - Map ForceError @ Log -- apply Errors to Log
 -
 -}

forceError_parseM :: PadsParser (LogEntry, (Base_md, LogEntry_imd))
forceError_parseM = do
  isError <- takeHeadStrP "[ ERROR ]"
  if isError
    then do
      (time, time_md) <- nextDashEntry_parseM int_parseM
      (sse, sse_md) <- subsystemError_parseM
      return (Error time sse,
              (cleanBasePD, Error_imd cleanBasePD time_md sse_md))
  else let leDef = def1 () :: LogEntry
           (mdHead, mdBody) = defaultMd1 () leDef
        in return (leDef, (mdHead { skipped = True }, mdBody))

map_parseM :: PadsParser (LogEntry, (Base_md, LogEntry_imd))
           -> PadsParser (Log, (Base_md, Log_imd))
map_parseM p = do
  arrayIsDone <- isEOFP -- check if the array is over
  if arrayIsDone
    then let lDef = def1 () :: Log
             lDef_md = defaultMd1 () lDef
          in return (Log [], lDef_md)
  else do
    (le, le_md@(le_md_head, le_md_body)) <- p
    takeHeadStrP "\n"
    (Log rest, (rest_md_head, Log_imd (h, mdList))) <- map_parseM p
    return (Log (le:rest),
             (le_md_head <> rest_md_head, Log_imd (h, (le_md:mdList)) ))

-- getErrorsDirectGen :: FilePath -> IO LogEntry
getErrorsDirectGen logFile = do
  src <- padsSourceFromFile logFile
  return . fst . fst . fst $ forceError_parseM # src

getErrorsDirect :: FilePath -> IO Log
getErrorsDirect logFile = do
  src <- padsSourceFromFile logFile
  let Log logData = fst . fst . fst $ log_parseM # src
      isError (Error _ _) = True
      isError _ = False
  return . Log $ (filter isError logData)

{- UC2: Skin to get all the errors between times N and M
 -
 - skin ForceDeltaError =
 -   defer case
 -       Error <| \timestamp -> do
 -                { put (timestamp >= N && timestamp <= M)
 -                ; pure timestamp
 -                }
 -              |>
 -             <| \sse -> do
 -                { inRange <- get
 -                ; if inRange then Force sse else Defer
 -                }
 -              |>
 -
 - Map MapErrors @ Log
 -
 - skin SkipServer = (defer, ForceDeltaError)
 - SkipServer @ ServerPrefixedLogEntry
 -
 - Questions:
 -   - How can we make sure that `sse` is never parsed when Defer is returned?
 -   - NOTE: this code is uncomfortably stateful, which depends on a parsing order.
 -           We might want to add `putLocal` and `getLocal` to the monad so that we can
 -           prevent stateful footguns.
 -
 -
 -}

-- Generated code for: [pads|
--   newtype SimpleLog = SimpleLog ([SimpleLogEntry | EOR] terminator EOF)
--   type SimpleLogEntry = (StringFW 20, Int)
--   skin CountInts =
--       (defer, <| \i -> get >>= \count -> put (i + count) >> force i |>)
--   skin CountInts' =
--       (defer, <| \i s -> (Force i, s + i) |>)
--   Map CountInts @ SimpleLog
--
-- An alternative formulation which does not rely on thunks is to make
-- defer a keyword, and force just sugar for the identiy function (modulo
-- a tuple constructor).
--
--   skin CountInts'' =
--       (defer, <| \i s -> (Keep i, s + i) |>)
-- |]

-- the generated code for use in the monadic interface.
countInts_parseFoldM ::
  Int -> PadsParser ((StringFW, Int), (Base_md, (Base_md, Base_md)), Int)
countInts_parseFoldM st0 = do
  -- skip the string prefix
  (stringfw_parse, stringfw_parse_md) <- stringFW_parseM 20
  let (stringfw, stringfw_md, st1) =
        case runPadsState ((const defer') stringfw_parse) st0 of
          (ForceM parseResult, st') -> (parseResult, stringfw_parse_md, st')
          (DeferM _, st') -> ("", mempty, st')

  -- parse the int and pass it in to the user-provided function
  (int_parse, int_parse_md) <- int_parseM
  let userBlob :: Int -> PadsParseState Int Int
      userBlob = \i -> get >>= \count -> put (i + count) >> force' i
      (int, int_md, st2) =
        case runPadsState (userBlob int_parse) st1 of
          (ForceM parseResult, st') -> (parseResult, int_parse_md, st')
          (DeferM _, st') -> (0, mempty, st')

  return ((stringfw, int), (mempty, (stringfw_md, int_md)), st2)

data Force a = Force a
             | Defer
-- the generated code for the raw function case. Not using the monadic interface.
countInts_parseFoldM' ::
  Int -> PadsParser ((StringFW, Int), (Base_md, (Base_md, Base_md)), Int)
countInts_parseFoldM' st0 = do
  -- skip the string prefix
  (stringfw_parse, stringfw_parse_md) <- stringFW_parseM 20
  let (stringfw, stringfw_md, st1) =
        case (\_ s -> (Defer, s)) stringfw_parse st0 of
          (Force parseResult, st') -> (parseResult, stringfw_parse_md, st')
          (Defer, st') -> ("", mempty, st')

  -- parse the int and pass it in to the user-provided function
  (int_parse, int_parse_md) <- int_parseM
  let (int, int_md, st2) =
        case (\i s -> (Force i, s + i)) int_parse st1 of
          (Force parseResult, st') -> (parseResult, int_parse_md, st')
          (Defer, st') -> (0, mempty, st')

  return ((stringfw, int), (mempty, (stringfw_md, int_md)), st2)

-- the generated code for the raw function case. Not using the monadic interface.
countInts_parseFoldM'' ::
  Int -> PadsParser ((StringFW, Int), (Base_md, (Base_md, Base_md)), Int)
countInts_parseFoldM'' st0 = do
  -- skip the string prefix
  primPads $ \s -> ((), (snd . S.takeBytes 10) s)
  let (stringfw, stringfw_md) =
        ("", mempty `modifyMdHeader` (\h -> h { skipped = True }))

  -- parse the int and pass it in to the user-provided function
  (int_parse, int_md_init) <- int_parseM
  let (int_res, st2) = (\i s -> (Keep i, s + i)) int_parse st0
      (int, int_md) = case int_res of
                          Keep i -> (i, int_md_init)
                          Discard -> (0, int_md_init `modifyMdHeader` (\h -> h { skipped = True }))


  return ((stringfw, int), (mempty, (stringfw_md, int_md)), st2)




map_CountInts_parseM ::
  PadsParser (SimpleLog, (Base_md, SimpleLog_imd))
map_CountInts_parseM = undefined
-- skipServer_ServerPrefixedLogEntry_parseM
--   :: PadsParser ((StringFW, LogEntry), (Base_md, (Base_md, (Base_md, LogEntry_imd))))
-- skipServer_ServerPrefixedLogEntry_parseM = do
--   -- skip the prefix
--   primPads $ \s -> ((), (snd . S.takeBytes 10) s)
--   undefined



--
-- Parse aggregation sets. Do we want to provide the ability to have several
-- different state variables? An example of what the surface syntax and the
-- corresponding generated code might look like is below. (Using the Keep/Discard
-- form from above).
-- [pads|
--   newtype BasicLog = BasicLog ([BasicLogEntry | EOR] terminator EOF)
--   type BasicLogEntry = (StringFW 20, Int, ':', Double, ':', Int)
--
--   skin SumIntsMaxDoubles = ( defer
--                            , intSum <-| \(i, s) -> (Discard, s + i) |>
--                            , defer
--                            , doubleMax <-| \(d, m) -> (Discard, if d > m then d else m) |>
--                            , defer
--                            , intSum <-| \(i, s) -> (Discard, s + i) |> )
--
--   skin SumAll = (defer
--                 , <| \(i, s) -> (Disgard, s + (fromIntegral i)) |>
--                 , defer
--                 , <| \(d, s) -> (Disgard, d + s) |>
--                 , defer
--                 , <| \(i, s) -> (Disgard, s + (fromIntegral i)) |>)
-- |]
--
-- If you want different state variables (implimented as fields in a record), you can
-- give them labels. I changed the syntax for embedding a haskell expression in this
-- case to have a back arrow <-| to try to make it clear that you are mutating the
-- given field in the state record. In the case were you only want one state variable,
-- PADS will not generate a record to wrap it, and you don't have to provide any labels
-- (making this backwards compatable with the code above).
--

data SumIntsMaxDoublesState = SumIntsMaxDoublesState {
    -- will we need explicit type annotations for this? I'm not sure how to infer these types in general.
    intSum :: Int
  , doubleMax :: Double
  }
sumIntsMaxDoubles_parseFoldM ::
  SumIntsMaxDoublesState -> PadsParser ( (StringFW, Int, Double, Int)
                                       , (Base_md, (Base_md, Base_md, Base_md, Base_md))
                                       , SumIntsMaxDoublesState)
sumIntsMaxDoubles_parseFoldM st0 = do
  -- skip the string prefix
  primPads $ \s -> ((), (snd . S.takeBytes 10) s)
  let (stringfw, stringfw_md) =
        ("", mempty `modifyMdHeader` (\h -> h { skipped = True }))
      st1 = st0

  -- parse the int and pass it in to the user-provided function, then modify the state field
  (int1_parse, int1_md_init) <- int_parseM
  let (int1_res, int1_st) = (\i s -> (Discard, s + i)) int1_parse (intSum st0)
      (int1, int1_md) = case int1_res of
                          Keep i -> (i, int1_md_init)
                          Discard -> (0, int1_md_init `modifyMdHeader` (\h -> h { skipped = True }))
      st2 = st1 { intSum = int1_st }

  -- skip the colon
  primPads $ \s -> ((), (snd . S.takeBytes 1) s)

  -- parse the double and pass it in to the user-provided function, then modify the state field
  (double_parse, double_md_init) <- double_parseM
  let (double_res, double_st) = (\d m -> (Discard, if d > m then d else m)) double_parse (doubleMax st0)
      (double, double_md) = case double_res of
                          Keep i -> (i, double_md_init)
                          Discard -> (0, double_md_init `modifyMdHeader` (\h -> h { skipped = True }))
      st3 = st2 { doubleMax = double_st }

  -- skip the colon
  primPads $ \s -> ((), (snd . S.takeBytes 1) s)

  -- parse the int and pass it in to the user-provided function, then modify the state field
  (int2_parse, int2_md_init) <- int_parseM
  let (int2_res, int2_st) = (\i s -> (Discard, s + i)) int2_parse (intSum st0)
      (int2, int2_md) = case int2_res of
                          Keep i -> (i, int2_md_init)
                          Discard -> (0, int2_md_init `modifyMdHeader` (\h -> h { skipped = True }))
      st4 = st3 { intSum = int2_st }

  return ((stringfw, int1, double, int2), (mempty, (stringfw_md, int1_md, double_md, int2_md)), st4)

--
-- The generated code for SumAll would look just like the generated code for
--

--   skin SumAll = (defer
--                 , <| \(i, s) -> (Disgard, s + (fromIntegral i)) |>
--                 , defer
--                 , <| \(d, s) -> (Disgard, d + s) |>
--                 , defer
--                 , <| \(i, s) -> (Disgard, s + (fromIntegral i)) |>)







getErrorsDelta :: FilePath -> Int -> Int -> IO Log
getErrorsDelta logFile n m = do
  src <- padsSourceFromFile logFile
  let Log logData = fst . fst . fst $ log_parseM # src
      isError (Error _ _) = True
      isError _ = False
      and = liftA2 (&&)
  return . Log $ filter (((>=n) `and` (<=m)) . time) (filter isError logData)

-- getErrorsDirect :: FilePath -> IO [LogEntry]
-- getErrorsDirect logFile = do
--   src <- padsSourceFromFile logFile
--   let logData = fst . fst . fst $ log_parseM # src
--       isError (Error _ _) = True
--       isError _ = False
--   return (filter isError logData)

{- UC3: Skin to count all the errors
 -
 - skin CountIfError =
 -  defer case
 -      Error <| get >>= \count -> put (count + 1) >> Defer |> defer
 -
 - Map CountIfError @ Log -- apply Errors to Log
 -
 -
 - Questions/Concerns:
 -    - This feels like a hack. What if Error has no arguments?
 -           | Error <| get >>= \c -> put (c + 1) |>
 -    - What problems arise from letting the user functions return Force
 -      and Defer to indicate if members should be parsed? Would this
 -      cause issues with lazyness? Would actual parsing only go far enough
 -      to give us access to the bit we want, or do we need to make this
 -      process a bit more explicit?
 -
 -}

countErrors :: FilePath -> IO Int
countErrors logFile = do
  src <- padsSourceFromFile logFile
  let Log logData = fst . fst . fst $ log_parseM # src
      isError (Error _ _) = True
      isError _ = False
  return . length . filter isError $ logData

{- UC4: Sum the # of bytes from host=example.com
 -
 - skin SumBytes =
 -   defer case
 -       Info defer
 -          (defer case
 -             | TSInfoPacketReceived
 -                  defer
 -                  <| \bytes -> do { (currentCount, _) <- get
 -                                  ; put (currentCount, bytes)
 -                                  ; pure bytes
 -                                  } |>
 -                  <| \host -> do
 -                        { (count, bytes) <- get
 -                        ; when (host == "example.com") $ put (count, 0)
 -                        ; pure host
 -                        }
 -                  defer) |>
 -
 - Map SumBytes @ Log -- apply Errors to Log
 -
 -
 - Notes:
 -   - This is awefully stateful. Is there some way to provide ways for
 -     earlier steps to stuff values into variables? Perhaps any variable
 -     could be the same as `force` and `_` could mean `defer`.
 -
 -}

countBytesFromHost :: FilePath -> String -> IO Int
countBytesFromHost logFile hostname = do
  src <- padsSourceFromFile logFile
  let Log logData = fst . fst . fst $ log_parseM # src
      numBytes (Info _ (SSInfoTransport (TSInfoPacketReceived _ bytes host _)))
         | host == hostname = bytes
      numBytes _ = 0
  return . sum . map numBytes $ logData


--
--
--

genTestFile :: FilePath -> Int -> IO ()
genTestFile file numLines = withFile file WriteMode mkFile
    where mkFile handle = do
            let addNLines 0 _ _ = return ()
                addNLines linesLeft gen time = do
                  logLine <- generate arbitrary :: IO LogEntry
                  let (tDelta, gen') = randomR (0, 10) gen
                      time' = time + tDelta
                      logLineStamped = liSetTime time' logLine
                  hPutStrLn handle ((pretty 200 . ppr) logLineStamped)
                  addNLines (linesLeft - 1) gen' time'
            gen <- newStdGen
            addNLines numLines gen 0

liSetTime :: Int -> LogEntry -> LogEntry
liSetTime t (Warning _ x) = Warning t x
liSetTime t (Error _ x) = Error t x
liSetTime t (Info _ x) = Info t x

--
-- These arbitrary instances are mostly to be able to auto-generate
-- lots of test data, but they have the added benifit of making
-- testing real easy.
--

instance Arbitrary Log where
  arbitrary = Log <$> listOf1 arbitrary

instance Arbitrary LogEntry where
  arbitrary = oneof [
      Info <$> arbitrary <*> arbitrary
    , Warning <$> arbitrary <*> arbitrary
    , Error <$> arbitrary <*> arbitrary
    ]

instance Arbitrary SubsystemWarning where
  arbitrary = oneof [
      SSWarnTransport <$> arbitrary
    , SSWarnRender <$> arbitrary
    ]
instance Arbitrary SubsystemError where
  arbitrary = oneof [
      SSErrorTransport <$> arbitrary
    , SSErrorRender <$> arbitrary
    ]
instance Arbitrary SubsystemInfo where
  arbitrary = oneof [
      SSInfoTransport <$> arbitrary
    , SSInfoRender <$> arbitrary
    ]


instance Arbitrary TransportInfo where
  arbitrary = oneof [
      do { msg <- readableString
         ; return $ TSInfoUnstructured (length msg) msg
         }
    , do { host <- oneof [return "example.com", readableString]
         ; port <- arbitrary `suchThat` (>0) :: Gen Int
         ; bytes <- arbitrary `suchThat` (>0) :: Gen Int
         ; return $ TSInfoPacketReceived
                      (5 + (length . show) bytes +
                       6 + 11 + length host + 1 + (length . show) port)
                      bytes host port
         }
    ]
instance Arbitrary TransportError where
  arbitrary = oneof [
      do { msg <- readableString
         ; return $ TSErrorUnstructured (length msg) msg
         }
    ]
instance Arbitrary TransportWarning where
  arbitrary = oneof [
      do { msg <- readableString
         ; return $ TSWarnUnstructured (length msg) msg
         }
    , do { host <- readableString
         ; port <- arbitrary :: Gen Int
         ; return $ TSWarnConnectionFailed
                      (length host + 1 + (length . show) port)
                      host port
         }
    , do { cb <- readableString
         ; hasReason <- arbitrary :: Gen Bool
         ; reason <-
           if hasReason
              then Just <$> readableString
              else return Nothing
         ; return $ TSWarnRealTimeTimeout
                      (length cb + 1 + maybe 0 length reason + 1)
                      cb reason
         }
    ]

instance Arbitrary RenderInfo where
  arbitrary = oneof [
      do { msg <- readableString
         ; return $ RDInfoUnstructured (length msg) msg
         }
    ]
instance Arbitrary RenderError where
  arbitrary = oneof [
      do { msg <- readableString
         ; return $ RDErrorUnstructured (length msg) msg
         }
    , do { errno <- arbitrary :: Gen Int
         ; message <- readableString
         ; return $ RDErrorScreenNotFound
                      (7 + (length . show) errno + 9 + length message)
                      errno message
         }
    ]
instance Arbitrary RenderWarning where
  arbitrary = oneof [
      do { msg <- readableString
         ; return $ RDWarnUnstructured (length msg) msg
         }
    , do { box1 <- arbitrary :: Gen Box
         ; box2 <- arbitrary :: Gen Box
         ; bogusLength <- arbitrary :: Gen Int -- too hard
         ; return $ RDWarnBoxConflict bogusLength box1 box2
         }
    ]

instance Arbitrary Box where
  arbitrary = Box <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

readableString :: Gen String
readableString = listOf . elements $
  "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789 ~!@#$%^&*(){}[]|\\\";<>,.?/`"


--
-- Pretty Printing
--
-- It seems like PADS should codegen this, but when I to use the
-- *_printFL functions they did not work. This was easier.
--

bar = Log [Error {time = -3, subsystemError = SSErrorRender (RDErrorScreenNotFound {rdSNFLen = 21, rdSNFErrNo = 4, rdSNFMessage = "u9\\N"})},Warning {time = -1, subsystemWarning = SSWarnRender (RDWarnUnstructured {rdUWLen = 1, rdUWMessage = "b"})},Info {time =2, subsystemInfo = SSInfoRender (RDInfoUnstructured {rdUILen = 0, rdUIMessage = ""})}]

barS = "[ ERROR ] - -3 - render - :screen-not-found:21: errno=4 message=u9\\N\n[ WARN ] - -1 - render - :unstructured:1:b\n[ INFO ] - 2 - render - :unstructured:0:"

barS' = "[ ERROR ] - -3 - render - :screen-not-found:21: errno=4 message=u9\\N\n"

foo = Log [Error {time = -1, subsystemError = SSErrorRender (RDErrorScreenNotFound {rdSNFLen = 18, rdSNFErrNo = -2, rdSNFMessage = ""})},Error {time = -2, subsystemError = SSErrorTransport (TSErrorUnstructured {tsUELen = 3, tsUEMessage = "PR!"})},Info {time = -3, subsystemInfo = SSInfoRender (RDInfoUnstructured {rdUILen = 0, rdUIMessage = ""})}]


fooS = "[ ERROR ] - -1 - render - :screen-not-found:18: errno=-2 message=\n[ ERROR ] - -2 - transport - :unstructured:3:PR!\n[ INFO ] - -3 - render - :unstructured:0:"


instance Pretty Log where
  ppr (Log l) = foldr1 (<>) $ punctuate (char '\n') (map ppr l)

instance Pretty LogEntry where
  ppr (Info time ssi) = text "[ INFO ] - " <> ppr time <> ppr ssi
  ppr (Warning time ssi) = text "[ WARN ] - " <> ppr time <> ppr ssi
  ppr (Error time ssi) = text "[ ERROR ] - " <> ppr time <> ppr ssi

-- subsystems
instance Pretty SubsystemWarning where
  ppr (SSWarnTransport x) = text " - transport - " <> ppr x
  ppr (SSWarnRender x) = text " - render - " <> ppr x
instance Pretty SubsystemError where
  ppr (SSErrorTransport x) = text " - transport - " <> ppr x
  ppr (SSErrorRender x) = text " - render - " <> ppr x
instance Pretty SubsystemInfo where
  ppr (SSInfoTransport x) = text " - transport - " <> ppr x
  ppr (SSInfoRender x) = text " - render - " <> ppr x


-- render messages
instance Pretty RenderInfo where
  ppr (RDInfoUnstructured len payload) =
    text ":unstructured:" <> ppr len <> text (':' : payload)
instance Pretty RenderError where
  ppr (RDErrorUnstructured len payload) =
    text ":unstructured:" <> ppr len <> text (':' : payload)
  ppr (RDErrorScreenNotFound len errno message) =
    text ":screen-not-found:" <> ppr len
       <> text ": errno=" <> ppr errno
       <> text " message=" <> ppr message
instance Pretty RenderWarning where
  ppr (RDWarnUnstructured len payload) =
    text ":unstructured:" <> ppr len <> text (':' : payload)
  ppr (RDWarnBoxConflict len box1 box2) =
    text ":box-conflict:" <> ppr len
       <> text ": box1=" <> ppr box1
       <> text " conflicts with box2=" <> ppr box2
instance Pretty Box where
  ppr (Box ul ur lr ll) = ptPpr ul `dash` ptPpr ur `dash` ptPpr lr `dash` ptPpr ll
    where ptPpr (x, y) = char '(' <> ppr x <> char ',' <> ppr y <> char ')'
          dash f s = f <> char '-' <> s

-- transport messages
instance Pretty TransportWarning where
  ppr (TSWarnUnstructured len payload) =
    text ":unstructured:" <> ppr len <> text (':' : payload)
  ppr (TSWarnConnectionFailed len host port) =
    text ":connect-failed:" <> ppr len <> text (':' : host)
       <> char ':' <> ppr port
  ppr (TSWarnRealTimeTimeout len computeBlock reason) =
    text ":realtime-timeout:" <> ppr len
      <> text (':' : computeBlock) <> char ':'
      <> maybe empty (text . (++":")) reason
instance Pretty TransportError where
  ppr (TSErrorUnstructured len payload) =
    text ":unstructured:" <> ppr len <> text (':' : payload)
instance Pretty TransportInfo where
  ppr (TSInfoUnstructured len payload) =
    text ":unstructured:" <> ppr len <> text (':' : payload)
  ppr (TSInfoPacketReceived len bytes host port) =
    text ":packet-received:" <> ppr len <> text ": got "
    <> ppr bytes <> text " bytes from host="
    <> ppr host <> char ':' <> ppr port

tests :: IO ()
tests = do
  putStrLn "RenderInfo"
  quickCheck $ idempotant (fst . fst . renderInfo_parseS)
  putStrLn "RenderError"
  quickCheck $ idempotant (fst . fst . renderError_parseS)
  putStrLn "Box"
  quickCheck $ idempotant (fst . fst . box_parseS)
  putStrLn "RenderWarning"
  quickCheck $ idempotant (fst . fst . renderWarning_parseS)

  putStrLn "TransportWarning"
  quickCheck $ idempotant (fst . fst . transportWarning_parseS)
  putStrLn "TransportError"
  quickCheck $ idempotant (fst . fst . transportInfo_parseS)
  putStrLn "TransportInfo"
  quickCheck $ idempotant (fst . fst . transportInfo_parseS)

  putStrLn "SubsystemWarning"
  quickCheck $ idempotant (fst . fst . subsystemWarning_parseS)
  putStrLn "SubsystemInfo"
  quickCheck $ idempotant (fst . fst . subsystemInfo_parseS)
  putStrLn "SubsystemError"
  quickCheck $ idempotant (fst . fst . subsystemError_parseS)

  putStrLn "LogEntry"
  quickCheck $ idempotant (fst . fst . logEntry_parseS)

  putStrLn "Log"
  quickCheck $ idempotant (fst . fst . log_parseS)

-- | for createing idempotant property checks
idempotant :: (Pretty a, Eq a) => (String -> a) -> a -> Bool
idempotant parseFun x = x == ((parseFun . pretty 200 . ppr) x)

