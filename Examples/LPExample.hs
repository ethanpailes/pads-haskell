{-# LANGUAGE TypeFamilies, TemplateHaskell, QuasiQuotes, MultiParamTypeClasses, FlexibleInstances, DeriveDataTypeable, ScopedTypeVariables, TypeSynonymInstances #-}

module Examples.LPExample where

import Language.Pads.Padsc
import Language.Pads.Generic

import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen

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

unstructuredWarning =
  logEntry_parseS "[ WARN ] - 234321423 - transport - :unstructured:4:xxxx"
connectFailedWarning =
  logEntry_parseS "[ WARN ] - 234321423 - transport - :connect-failed:14:example.com:80"

boxCollideWarning =
  logEntry_parseS $
        "[ WARN ] - 234321423 - render - :box-conflict:"
        ++ (show . length) payload ++ ":" ++ payload
    where payload = (" box1=(1,2)-(3,4)-(5,6)-(7,8) conflicts with "
                    ++ "box2=(9,10)-(11,12)-(13,14)-(15,16)")

screenNotFoundError =
  logEntry_parseS $
        "[ ERROR ] - 234321423 - render - :screen-not-found:"
        ++ (show . length) payload ++ ":" ++ payload
    where payload = " errno=-17 message=the rest of the message"



leDef :: LogEntry
leDef = def1 ()

leDefMd :: LogEntry_md
leDefMd = defaultMd1 () leDef

printLeDef :: LogEntry -> FList
printLeDef le = logEntry_printFL (le, leDefMd)

showLogEntry :: LogEntry -> String
showLogEntry = showFList . printLeDef

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
  "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789 ~!@#$%^&*(){}[]|\\\":;<>,.?/`"
