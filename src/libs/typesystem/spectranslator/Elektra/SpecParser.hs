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
    <*> getMeta k "default" Nothing      Just
    <*> getMeta k "type"    (Right ".*") parseType
    <*> pFunctionCandidates
    <*> parseTypeSpecification r k
  where
    pFunctionCandidates = keyListMeta k >>= mapM parseFunctionCandidate
    functionName fk = arrayValidateName fk >>= \isArray ->
      if isArray == Invalid
      then Function <$> keyName fk
      else do
        bfk         <- flip keyAddName "#" !=<< keyDeleteBaseName !=<< keyDup fk
        baseName    <- fmap T.pack (keyGetRelativeName bfk r)
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
    <*> getMeta k "elektra/spec/type"   Nothing parseTypeSignature
    <*> getMeta k "elektra/spec/impl"   Nothing parseImpl
    <*> getMeta k "elektra/spec/order"  5       read
    <*> getMeta k "elektra/spec/rename" Nothing Just 
  where
    isSeparator s = s == '\n' || s == ';'
    parseImpl = Just . fmap T.unpack . T.split isSeparator . T.pack

getMeta :: Key -> String -> a -> (String -> a) -> IO a
getMeta k m d p = keyGetMeta k m >>= ifKey (return d) (fmap p . keyString)

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
