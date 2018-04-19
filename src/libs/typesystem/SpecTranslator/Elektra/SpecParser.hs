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
  parseTypeSpecifications, parseKeySpecifications, resolvePath
) where

import Elektra.Key
import Elektra.KeySet
import Elektra.Ease

import Control.Applicative (pure, liftA2)
import Control.Monad       (filterM, liftM2, join, void)
import Data.List           (isPrefixOf, sort)
import Data.List.Split     (splitOn)
import System.IO.Unsafe    (unsafePerformIO)

import Elektra.Specifications
import Elektra.Parsers
import Elektra.Range
import FiniteAutomata

import Debug.Trace

import qualified Data.Text       as T
import qualified Data.Text.Read  as T

type RootKey = Key

-- we can safely use unsafePerformIO as the result only depends on the inputs
resolvePath :: String -> String -> String
resolvePath k s = unsafePerformIO $ do
  d <- keyNew $ "/" ++ k
  keyAddBaseName d s
  keyName d

specPrefix :: String -> String
specPrefix = flip (++) "/elektra/spec" 

specPrefixKey :: String -> IO Key
specPrefixKey = keyNew . specPrefix

keyFilterMeta :: Key -> (String -> Bool) -> IO [Key]
keyFilterMeta k p = keyListMeta k >>= filterM (fmap p . keyName) 

-- TODO presorted for now, but we really need to find some kind of general way
links :: [String]
links = ["fallback/#", "override/#"]
instance Ord FunctionCandidate
  where
    a <= b = let n1 = functionBaseName $ fncFun a
                 n2 = functionBaseName $ fncFun b
             in  if n1 `elem` links then
                   if n2 `elem` links then 
                     n1 <= n2
                   else
                     True
                  else
                    False

parseKeySpecification :: RootKey -> Key -> IO KeySpecification
parseKeySpecification r k = do
  pPath               <- keyGetRelativeName k r
  pDefaultValue       <- ifKey (keyGetMeta k "default") (fmap Just . keyString) (return Nothing) 
  pKeyType            <- ifKey (keyGetMeta k "type") (fmap parseType . keyString) (return $ Right ".*")
  pFunctionCandidates <- let notReserved = liftA2 (&&) (/= "default") (/= "type") in mapM parseFunctionCandidate =<< keyFilterMeta k notReserved
  pTypeSpecification  <- parseTypeSpecification r k
  return $ KeySpecification pPath pDefaultValue pKeyType (sort pFunctionCandidates) pTypeSpecification
  where
    parseFunctionCandidate fk = do
      str     <- keyString fk
      pth     <- keyNew str >>= flip keyGetRelativeName r
      isArray <- arrayValidateName fk
      if isArray == Invalid
      then do
        name <- keyName fk
        return $ FunctionCandidate (Function name) pth str
      else do
        bfk          <- flip keyAddName "#" !=<< keyDeleteBaseName !=<< keyDup fk
        baseName     <- fmap T.pack (keyGetRelativeName bfk r)
        let splitted = T.splitAt (T.length baseName) baseName
        let arrFn    = ArrayFunction (T.unpack $ fst splitted) (T.unpack $ snd splitted)
        return $ FunctionCandidate arrFn pth str

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
