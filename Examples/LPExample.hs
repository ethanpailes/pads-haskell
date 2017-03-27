{-# LANGUAGE TypeFamilies, TemplateHaskell, QuasiQuotes, MultiParamTypeClasses, FlexibleInstances, DeriveDataTypeable, ScopedTypeVariables, TypeSynonymInstances #-}

module Examples.LPExample where

import Language.Pads.Padsc
import Language.Pads.Generic

import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen
import Test.QuickCheck
import Text.PrettyPrint.Mainland as PP
import System.IO (withFile, IOMode(WriteMode), hPutStrLn)
import System.Random (newStdGen, randomR)

[pads|
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

-- | the payloads for each of the major log categories
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
  , rdSNFMessage :: (" message=", [Char] terminator EOR)
  }

data RenderInfo =
    RDInfoUnstructured { ":unstructured:"
  , rdUILen :: Len
  , rdUIMessage :: StringFW rdUILen
  }

type NextDashEntry a = (" - ", a)
|]

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

-- | for createing idempotant property checks
idempotant :: (Pretty a, Eq a) => (String -> a) -> a -> Bool
idempotant parseFun x = x == ((parseFun . pretty 200 . ppr) x)

