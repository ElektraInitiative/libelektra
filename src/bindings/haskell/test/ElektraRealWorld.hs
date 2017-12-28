module Main (main) where

import Elektra.Key
import Elektra.KeySet
import Elektra.KDB
import Test.Hspec

main :: IO ()
main = hspec $
  describe "KDB"
  $ it "creates a new kdb connection, saves a key, closes the connection, reopens it and checks the stored key"
  $ do
    parent <- keyNew "/parent"
    ks <- ksNew 1
    _ <- kdbOpen parent $ \kdb -> do
      _ <- kdbGet kdb ks parent
      _ <- keyNew haskellPersisted >>= ksAppendKey ks
      kdbSet kdb ks parent
    ksAfter <- ksNew 1
    kdbOpen parent $ \kdbAfter -> do
      _ <- kdbGet kdbAfter ksAfter parent
      ksLookupByName ksAfter haskellPersisted >>= keyName >>= (`shouldBe` haskellPersisted)
  where
    haskellPersisted = "user/tests/testhaskell_cabal/haskellPersisted"
