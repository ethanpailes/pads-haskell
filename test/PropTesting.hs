{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | This module contains `Arbitrary` instances to allow
--   property testing for various values in PADS. They are orphan
--   instances, but this way we don't need to package QuickCheck
--   in the library provided to users.
module PropTesting where

import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen
import Language.Pads.Syntax
import Control.Monad (liftM2)
import Language.Pads.Parser (keywords)
import Data.List (intersperse)

instance Arbitrary PadsSkinPat where
  arbitrary = arbitraryPadsSkinPat 3

arbitraryPadsSkinPat :: Int -> Gen PadsSkinPat
arbitraryPadsSkinPat 0 = oneof [ return PSForce
                               , return PSDefer
                               ]
arbitraryPadsSkinPat n = oneof [
      return PSForce
    , return PSDefer
    , PSTupleP <$>
       (liftM2 (:))
           (arbitraryPadsSkinPat (n-1))
           (listOf1 (arbitraryPadsSkinPat (n-1)))
    , PSConP <$> qUpper <*> listOf1 (arbitraryPadsSkinPat (n-1))
    , PSRecP <$> qUpper
      <*> listOf1 (do { name <- lower
                      ; pat <- arbitraryPadsSkinPat (n-1)
                      ; return (name, pat)
                      })
    ]


--
-- Utilities
--

startsWith :: (Char -> Bool) -> Gen String
startsWith p = suchThat (do
  c <- suchThat (arbitrary :: Gen Char) p
  rest <- listOf (suchThat arbitrary (`elem` uppers ++ lowers))
  return (c:rest)) (not . (`elem` keywords))
upper :: Gen String
upper = startsWith (`elem`uppers)
lower :: Gen String
lower = startsWith (`elem`lowers)
qUpper :: Gen QString
qUpper = oneof [upper >>= \x -> return [x],
               (liftM2 (:)) upper (upper >>= \x -> return [x])]
qLower :: Gen QString
qLower = oneof [lower >>= \x -> return [x],
               (liftM2 (:)) lower (lower >>= \x -> return [x])]

uppers :: [Char]
uppers = "ABCDEFGHIJKLMNOPQRSTUVWYXZ"
lowers :: [Char]
lowers = "abcdefghijklmnopqrstuv"
