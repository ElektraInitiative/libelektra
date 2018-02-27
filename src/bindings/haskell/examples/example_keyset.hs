--
-- @file
--
-- @brief KeySet Haskell bindings examples
--
-- @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
-- 
module Example (main) where

import Elektra.Key
import Elektra.KeySet
import Control.Monad
import Text.Printf

putEmptyLn :: IO ()
putEmptyLn = putStrLn ""

main :: IO ()
main = do
  ks1 <- ksNew 100
  key1 <- keyNew "user/key1"
  _ <- ksAppendKey ks1 key1
  _ <- keyNew "user/key2" >>= ksAppendKey ks1
  _ <- keyNew "user/key3" >>= ksAppendKey ks1

  ksGetSize ks1 >>= printf "KeySet1 has %d keys\n"
  putEmptyLn

  putStrLn "We can easily iterate over the keyset to check out its content:"
  ksList ks1 >>= mapM_ print -- or `print ks1` as KeySet is an instance of Show
  putStrLn "This works the other direction too:"
  ksList ks1 >>= mapM_ print . reverse
  putEmptyLn

  putStrLn "We can check if a key is in a keyset:"
  ksLookupByName ks1 "user/key1" >>= keyPtrNull >>= printf "  Is user/key1 in KeySet1? %s\n" . show . not
  putStrLn "This works with Key objects too:"
  ksLookup ks1 key1 >>= keyPtrNull >>= printf "  Is Key(user/key1) in KeySet1? %s\n" . show . not
  putEmptyLn

  putStrLn "We can create shallow copies and remove keys without affecting other keysets:"
  ks2 <- ksDup ks1
  _ <- ksPop ks2
  liftM2 (printf "  KeySet2 now has %d keys while KeySet1 still has %d keys") (ksGetSize ks2) (ksGetSize ks1)
    >>= putStrLn
  putEmptyLn

  putStrLn "In Haskell we can easily create deep copies and modify the keys inside:"
  _ <- ksHead ks1 >>= (flip . flip keySetMeta) "foo" "bar"
  ks3 <- ksGetSize ks1 >>= ksNew
  ksList ks1 >>= mapM_ (>>= ksAppendKey ks3) . fmap keyDup
  _ <- ksHead ks3 >>= (flip . flip keySetMeta) "foo" "changed"
  ksHead ks1 >>= flip keyGetMeta "foo" >>= keyString >>= printf "  KeySet1 has metadata %s\n"
  ksHead ks3 >>= flip keyGetMeta "foo" >>= keyString >>= printf "  KeySet3 has metadata %s\n"
