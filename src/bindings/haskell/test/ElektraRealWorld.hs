module Main where

import Elektra.Key
import Elektra.KeySet
import Elektra.KDB
import Test.Hspec

main :: IO ()
main = hspec $ do
    describe "KDB" $ it "creates a new kdb connection, saves a key, closes the connection, reopens it and checks the stored key" $ do
            parent <- keyNew "/parent"
            ks <- ksNew 1
            kdbOpen parent $ \kdb -> do
                kdbGet kdb ks parent
                keyNew haskellPersisted >>= ksAppendKey ks
                kdbSet kdb ks parent
            ksAfter <- ksNew 1
            kdbOpen parent $ \kdbAfter -> do
                kdbGet kdbAfter ksAfter parent
                test <- ksLookupByName ksAfter haskellPersisted
                name <- keyName test
                ksLookupByName ksAfter haskellPersisted >>= keyName >>= (`shouldBe` haskellPersisted)
    where
        haskellPersisted = "user/tests/testhaskell_cabal/haskellPersisted"
