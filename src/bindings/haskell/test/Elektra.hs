import Elektra.Key
import Elektra.KeySet
import Elektra.KDB
import Test.Hspec
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
  describe "KDB" $ it "creates a new kdb connection, saves a key, closes the connection, reopens it and checks the stored key" $ do
      parent <- keyNew "/parent"
      ks <- ksNew 1
      kdb <- kdbOpen parent
      kdbGet kdb ks parent
      keyNew haskellPersisted >>= ksAppendKey ks
      kdbSet kdb ks parent
      kdbClose kdb parent
      ksAfter <- ksNew 1
      kdbAfter <- kdbOpen parent
      kdbGet kdbAfter ksAfter parent
      test <- ksLookupByName ksAfter haskellPersisted KdbONone
      name <- keyName test
      ksLookupByName ksAfter haskellPersisted KdbONone >>= keyName >>= (`shouldBe` haskellPersisted)
  where
    name = "/tests/testhaskell_cabal"
    otherName = "/tests/testhaskell_cabal/other"
    other = "other"
    haskellPersisted = "user/tests/testhaskell_cabal/haskellPersisted"

testSingleKeyOp name fn expected = keyNew name >>= fn >>= (`shouldBe` expected)
testKeyModOp name modFn testFn expected = keyNew name >>= (\x -> modFn x >> testFn x >>= (`shouldBe` expected))
testSingleKeySetOp size fn expected = ksNew size >>= fn >>= (`shouldBe` expected)

    -- TODO testing properties instead of examples
    --it "returns the correct name size for keyGetNameSize" $
    --  property $ \x xs -> head (x:xs) == (x :: Int)
