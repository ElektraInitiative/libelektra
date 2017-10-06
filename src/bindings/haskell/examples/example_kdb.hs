module Example where

import Elektra.Key
import Elektra.KeySet
import Elektra.KDB

main :: IO ()
main = do
    parent <- keyNew "user/MyApp"
    putStrLn "Opening the database."
    kdbOpen parent $ \kdb -> do
        ks <- ksNew 100
        kdbGet kdb ks parent
        key <- ifKey (ksLookupByName ks "user/MyApp/mykey") return (keyNew "user/MyApp/mykey" >>= \n -> ksAppendKey ks n >> return n)
        keySet key "new_value"
        kdbSet kdb ks parent
    putStrLn "The database gets closed automatically after the lambda has been executed."
