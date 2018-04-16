--
-- @file
--
-- @brief Parses a specification from a keyset into an intermediate representation
--
-- @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
-- 
{-# LANGUAGE TypeFamilies, TypeInType, TypeOperators, OverloadedStrings,
             ExistentialQuantification, GADTs, UndecidableInstances #-}

module Elektra.SpecParser (
  parseTypeSpecifications, parseKeySpecifications
) where

import Elektra.Key
import Elektra.KeySet
import Elektra.Ease

import Control.Applicative (pure, liftA2)
import Control.Monad       (filterM, liftM2, join, void)
import Data.List           (isPrefixOf)
import Data.List.Split     (splitOn)

import Elektra.Specifications
import Elektra.Parsers
import Elektra.Range
import FiniteAutomata

import qualified Data.Text       as T
import qualified Data.Text.Read  as T

type RootKey = Key

specPrefix :: String -> String
specPrefix = flip (++) "/elektra/spec" 

specPrefixKey :: String -> IO Key
specPrefixKey = keyNew . specPrefix

keyFilterMeta :: Key -> (String -> Bool) -> IO [Key]
keyFilterMeta k p = keyListMeta k >>= filterM (fmap p . keyName) 

parseKeySpecification :: RootKey -> Key -> IO KeySpecification
parseKeySpecification r k = do
  pPath               <- keyGetRelativeName k r
  pDefaultValue       <- ifKey (keyGetMeta k "default") (fmap Just . keyString) (return Nothing) 
  pKeyType            <- ifKey (keyGetMeta k "type") (fmap parseType . keyString) (return $ Right ".*")
  pFunctionCandidates <- let notReserved = liftA2 (&&) (/= "default") (/= "type") in mapM parseFunctionCandidate =<< keyFilterMeta k notReserved
  pTypeSpecification  <- parseTypeSpecification r k
  return $ KeySpecification pPath pDefaultValue pKeyType pFunctionCandidates pTypeSpecification
  where
    parseFunctionCandidate fk = do
      str     <- keyString fk >>= keyNew >>= flip keyGetRelativeName r
      isArray <- arrayValidateName fk
      if isArray == Invalid
      then do
        name <- keyName fk
        return (Function name, str)
      else do
        bfk          <- flip keyAddName "#" !=<< keyDeleteBaseName !=<< keyDup fk
        baseName     <- fmap T.pack (keyGetRelativeName bfk r)
        let splitted = T.splitAt (T.length baseName) baseName
        return (ArrayFunction (T.unpack $ fst splitted) (T.unpack $ snd splitted), str)

parseTypeSpecification :: RootKey -> Key -> IO TypeSpecification
parseTypeSpecification r k = do
  pName  <- keyGetRelativeName k r
  --pVar   <- parsePathVariable . T.pack <$> keyString k
  pImpl  <- let parseImpl = fmap (Just . lines) . keyString in
            ifKey (keyGetMeta k "elektra/spec/impl") parseImpl (return Nothing)
  pType  <- ifKey (keyGetMeta k "elektra/spec/type") (fmap parseTypeSignature . keyString) (return Nothing)
  -- TODO: improve error handling
  return $ TypeSpecification pName Nothing pType pImpl

parseType :: String -> Either Path String
parseType t
  | "./" `isPrefixOf` t || "/" `isPrefixOf` t = Left t
  | otherwise = Right t

parseSpecifications :: RootKey -> KeySet -> (Key -> IO a) -> (Key -> KeySet -> IO KeySet) -> IO [a]
parseSpecifications k ks parser keySelector = keySelector k ks >>= ksList >>= mapM parser

parseKeySpecifications :: RootKey -> KeySet -> IO [KeySpecification]
parseKeySpecifications k ks = parseSpecifications k ks (parseKeySpecification k) (\k ks -> cutSpecElektra k ks >> return ks)

parseTypeSpecifications :: RootKey -> KeySet -> IO [TypeSpecification]
parseTypeSpecifications k ks = do
  spk <- keyName k >>= specPrefixKey
  parseSpecifications k ks (parseTypeSpecification spk) cutSpecElektra

cutSpecElektra :: Key -> KeySet -> IO KeySet
cutSpecElektra k ks = keyName k >>= specPrefixKey >>= ksCut ks

dt :: (T.Text -> T.Text) -> String -> String
dt fn = T.unpack . fn . T.pack

(!=<<) :: Monad m => (a -> m b) -> m a -> m a
a !=<< b = b >>= (\b' -> do _ <- a b'; return b')
infixr 1 !=<<
