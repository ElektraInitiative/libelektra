module Main where

import Elektra.Key
import Elektra.KeySet
import Elektra.KDB
import Test.Hspec
import Test.QuickCheck
import Control.Monad

main :: IO ()
main = hspec $ do
    describe "Key" $ do
        it "does what i want" $ testSingleKeyOp name keyGetNameSize 25
        it "returns the correct name size" $ testSingleKeyOp name keyGetNameSize 25
        it "returns the correct full name size" $ testSingleKeyOp name keyGetFullNameSize 25
        it "returns the correct full name" $ testSingleKeyOp name keyGetFullName name
        it "returns the correct base name size" $ testSingleKeyOp name keyGetBaseNameSize 18
        it "returns the correct base name" $ testSingleKeyOp name keyBaseName "testhaskell_cabal"
        it "returns the correct unescaped name size" $ testSingleKeyOp name keyGetUnescapedNameSize 25
        it "returns the correct unescaped name" $ testSingleKeyOp name keyUnescapedName "\0tests\0testhaskell_cabal\0"
        it "returns the correct namespace" $ testSingleKeyOp name keyGetNamespace KeyNsCascading
        it "creates a new key successfully with the correct name" $ testSingleKeyOp name keyName name
        it "sets the keys name" $ testKeyModOp name (`keySetName` otherName) keyName otherName
        it "sets the base name" $ testKeyModOp (name ++ "/baseName") (`keySetBaseName` other) keyName (name ++ "/" ++ other)
        it "adds an escaped name" $ testKeyModOp name (`keyAddName` otherName) keyName (name ++ otherName)
        it "adds a base name" $ testKeyModOp name (`keyAddBaseName` "haskell") keyName (name ++ "/haskell")
        it "supports multiple operations on the same key" $ do
            key <- keyNew name
            keyAddBaseName key "haskell"
            keyName key >>= (`shouldBe` name ++ "/haskell")
            keyAddBaseName key "other"
            keyName key >>= (`shouldBe` name ++ "/haskell/other")
        it "creates arbitrary key names" $ property $ forAll genSafeString $ \s1 -> forAll genSafeString $ \s2 -> do
            key <- keyNew ('/' : s1)
            keyAddBaseName key s2
            keyBaseName key >>= (`shouldBe` s2)
            keyName key >>= (`shouldBe` accepted s1 s2)
    describe "KeySet" $ do
        it "creates a new empty keyset" $ testSingleKeySetOp 5 ksGetSize 0
        it "creates a duplicate keyset which is not the same" $ do
            key <- keyNew name
            ks <- ksNew 1
            ksAppendKey ks key
            ks2 <- ksDup ks
            join $ liftM2 shouldBe (ksGetSize ks) (ksGetSize ks2)
            ksPop ks
            join $ liftM2 shouldNotBe (ksGetSize ks) (ksGetSize ks2)
    where
        name = "/tests/testhaskell_cabal"
        otherName = "/tests/testhaskell_cabal/other"
        other = "other"
        accepted s1 s2
            | null s1 && null s2 = "/%"
            | null s1 = '/':s2
            | null s2 = '/':s1 ++ "/%"
            | otherwise = '/':s1 ++ '/':s2

testSingleKeyOp name fn expected = keyNew name >>= fn >>= (`shouldBe` expected)
testKeyModOp name modFn testFn expected = keyNew name >>= (\x -> modFn x >> testFn x >>= (`shouldBe` expected))
testSingleKeySetOp size fn expected = ksNew size >>= fn >>= (`shouldBe` expected)

genSafeString = listOf $ elements ['a'..'z']
