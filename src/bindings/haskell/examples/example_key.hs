module Example (main) where

import Elektra.Key
import Text.Printf

putEmptyLn :: IO ()
putEmptyLn = putStrLn ""

main :: IO ()
main = do
  key1 <- keyNewWithValue "user/key1" "some_value"
  printf "Key1 %s" (show key1)
  putStrLn "Every Key has properties. Some are read only, some are read+write."
  putStrLn "Properties of Key1:"
  keyName key1        >>= printf "  key1.name     = %s\n"
  keyString key1      >>= printf "  key1.value    = %s\n"
  keyGetBaseName key1 >>= printf "  key1.basename = %s\n"
  keyGetFullName key1 >>= printf "  key1.fullname = %s\n"
  putEmptyLn

  ret <- keySet key1 (5 :: Int) -- allows to set it to anything that is an instance of Show
  putStrLn $ "keySet returned " ++ show ret
  keyString key1 >>= printf "We changed the value of Key1. New value is %s.\n"
  putEmptyLn

  key2 <- keyDup key1
  printf "Key2 is a copy of Key1. Do they match? %s.\n" (show $ key1 == key2)
  putEmptyLn

  _ <- keySetName key1 "system/key1"
  keyName key1 >>= printf "We changed name of Key1. New name is %s.\n"
  printf "Do they still match? %s\n" (show $ key1 == key2)
  putEmptyLn

  _ <- keySetBaseName key1 "key1_changed"
  keyName key1 >>= printf "Changing the basename only is possible as well. New name is %s.\n"
  putEmptyLn

  _ <- keySetMeta key1 "foo" "bar"
  _ <- keySetMeta key1 "owner" "e1528532"
  _ <- keySetMeta key1 "comment" "this is my example key"
  putStrLn "Keys can have metadata. We can iterate over or fetch them by name."
  putStrLn "Meta data of Key1 with their values:"
  keyListMeta key1 >>= mapM_ print
  putStrLn "Remember: Metadata is returned as a Key object."
