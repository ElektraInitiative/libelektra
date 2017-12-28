module Example (main) where

import Elektra.Key
import Elektra.KeySet
import Elektra.KDB

main :: IO ()
main = do
  parent <- keyNew "user/MyApp"
  putStrLn "Opening the database."
  ret <- kdbOpen parent $ \kdb -> do
    ks  <- ksNew 100
    _   <- kdbGet kdb ks parent
    key <- ifKey (ksLookupByName ks "user/MyApp/mykey")
           return
           (keyNew "user/MyApp/mykey" >>= \n -> ksAppendKey ks n >> return n)
    _   <- keySet key "new_value"
    kdbSet kdb ks parent
  putStrLn $ "kdbOpen returned " ++ show ret
  putStrLn "The database gets closed automatically after the lambda has been executed."
