--
-- @file
--
-- @brief Parses a specification from a keyset into an intermediate representation
--
-- @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
-- 
{-# LANGUAGE OverloadedStrings #-}

module Elektra.SpecParser (
  parseTypeSpecifications, parseKeySpecifications, resolvePath
) where

import Elektra.Key
import Elektra.KeySet
import Elektra.Ease

import Control.Applicative (liftA2)
import Control.Monad       (filterM)
import Data.List           (isPrefixOf)
import System.IO.Unsafe    (unsafePerformIO)
import qualified Data.Text as T

import Elektra.Specifications
import Elektra.Parsers

type RootKey = Key

-- we can safely use unsafePerformIO as the result only depends on the inputs
resolvePath :: String -> String -> String
resolvePath k s = unsafePerformIO $ do
  d <- keyNew $ "/" ++ k
  _ <- keyAddBaseName d s
  keyName d

specPrefix :: String -> String
specPrefix = flip (++) "/elektra/spec" 

specPrefixKey :: String -> IO Key
specPrefixKey = keyNew . specPrefix

keyFilterMeta :: Key -> (String -> Bool) -> IO [Key]
keyFilterMeta k p = keyListMeta k >>= filterM (fmap p . keyName) 

parseKeySpecification :: RootKey -> Key -> IO KeySpecification
parseKeySpecification r k = KeySpecification
    <$> keyGetRelativeName k r
    <*> pDefault
    <*> (keyGetMeta k "type" >>= ifKey (return $ Right ".*") (fmap parseType . keyString))
    <*> pFunctionCandidates
    <*> parseTypeSpecification r k
  where
    pDefault = keyGetMeta k "default" >>= ifKey (return Nothing) (fmap Just . keyString)
    pFunctionCandidates = let notReserved = liftA2 (&&) (/= "default") (/= "type")
                          in keyFilterMeta k notReserved >>= mapM parseFunctionCandidate
    functionName fk = arrayValidateName fk >>= \isArray ->
      if isArray == Invalid
      then Function <$> keyName fk
      else do
        bfk          <- flip keyAddName "#" !=<< keyDeleteBaseName !=<< keyDup fk
        baseName     <- fmap T.pack (keyGetRelativeName bfk r)
        let splitted = T.splitAt (T.length baseName) baseName
        return $ ArrayFunction (T.unpack $ fst splitted) (T.unpack $ snd splitted)
    parseFunctionCandidate fk = let str = keyString fk in FunctionCandidate 
        <$> functionName fk
        <*> (str >>= keyNew >>= flip keyGetRelativeName r)
        <*> str

parseTypeSpecification :: RootKey -> Key -> IO TypeSpecification
parseTypeSpecification r k = TypeSpecification
    <$> keyGetRelativeName k r
    <*> pure Nothing
    <*> pTypeSig
    <*> pImpl
    <*> pOrder
  where
    pTypeSig = keyGetMeta k "elektra/spec/type" >>= ifKey (return Nothing) (fmap parseTypeSignature . keyString)
    pImpl = let isSeparator s = s == '\n' || s == ';'
                parseImpl = fmap (Just . fmap T.unpack . T.split isSeparator . T.pack) . keyString
            in  (keyGetMeta k "elektra/spec/impl" >>= ifKey (return Nothing) parseImpl)
    pOrder = keyGetMeta k "elektra/spec/order" >>= ifKey (return 0) (fmap read . keyString)

parseType :: String -> Either Path String
parseType t
  | "./" `isPrefixOf` t || "/" `isPrefixOf` t = Left t
  | otherwise = Right t

parseSpecifications :: RootKey -> KeySet -> (Key -> IO a) -> (Key -> KeySet -> IO KeySet) -> IO [a]
parseSpecifications k ks parser keySelector = keySelector k ks >>= ksList >>= mapM parser

parseKeySpecifications :: RootKey -> KeySet -> IO [KeySpecification]
parseKeySpecifications k ks = parseSpecifications k ks (parseKeySpecification k) (\k' ks' -> cutSpecElektra k' ks' >> return ks')

parseTypeSpecifications :: RootKey -> KeySet -> IO [TypeSpecification]
parseTypeSpecifications k ks = do
  spk <- keyName k >>= specPrefixKey
  parseSpecifications k ks (parseTypeSpecification spk) cutSpecElektra

cutSpecElektra :: Key -> KeySet -> IO KeySet
cutSpecElektra k ks = keyName k >>= specPrefixKey >>= ksCut ks

(!=<<) :: Monad m => (a -> m b) -> m a -> m a
a !=<< b = b >>= (\b' -> do _ <- a b'; return b')
infixr 1 !=<<
