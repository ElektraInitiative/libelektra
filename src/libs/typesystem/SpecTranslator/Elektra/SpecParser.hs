--
-- @file
--
-- @brief Parses a specification from a keyset into an intermediate representation
--
-- @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
-- 
{-# LANGUAGE TypeFamilies, TypeInType, TypeOperators, 
             ExistentialQuantification, GADTs, UndecidableInstances #-}

module Elektra.SpecParser (
  parseTypeSpecifications, parseKeySpecifications, 
  Path, BaseType, Implementation, TypeName, FunctionCandidate, PathVariable (..), Function (..),
  TypeParameter (..), TypeSpecification (..), 
  TransformationSpecification (..), KeySpecification (..), functionBaseName
) where

import Elektra.Key
import Elektra.KeySet
import Elektra.Ease

import Control.Monad   (filterM, liftM2, join)
import Data.List       (isPrefixOf)
import Data.List.Split (splitOn)

import qualified Data.Text as T

prefix :: String
prefix = "spec/examples/"

specPrefix :: String
specPrefix = prefix ++ "specElektra"

prefixKey :: IO Key
prefixKey = keyNew prefix

specPrefixKey :: IO Key
specPrefixKey = keyNew specPrefix

type Path = String
type BaseType = String
type Implementation = String
type TypeName = String

type FunctionCandidate = (Function, String)
data Function = ArrayFunction TypeName String | Function TypeName deriving Eq

instance Show Function
  where show (ArrayFunction s i) = s ++ i
        show (Function s)        = s

functionBaseName :: Function -> String
functionBaseName (ArrayFunction s _) = s
functionBaseName (Function s)        = s

data PathVariable = Self | Path Path deriving Eq
instance Show PathVariable
  where
    show Self     = "self"
    show (Path p) = p

data TransformationSpecification = TransformationSpecification {
  transformFrom   :: Path,
  transformToType :: String
} deriving (Show, Eq)

data KeySpecification = KeySpecification {
  path               :: Path,
  defaultValue       :: Maybe String,
  keyType            :: Either Path String,
  functionCandidates :: [FunctionCandidate],
  typeSpecification  :: TypeSpecification
} deriving (Show, Eq)

-- TODO remove none, use some better error handling (errorT monad?)
data TypeParameter = TypeParameter { 
  typePathVariable :: PathVariable,
  baseType         :: BaseType
} deriving (Show, Eq)

data TypeSpecification = TypeSpecification {
  typeName       :: TypeName,
  pathVariable   :: PathVariable,
  typeParameters :: [TypeParameter],
  implementation :: Maybe [Implementation]
} deriving (Show, Eq)

keyFilterMeta :: Key -> (String -> Bool) -> IO [Key]
keyFilterMeta k p = keyListMeta k >>= filterM (fmap p . keyName) 

parseKeySpecification :: Key -> IO KeySpecification
parseKeySpecification k = do
  pPath               <- prefixKey >>= keyGetRelativeName k
  pDefaultValue       <- ifKey (keyGetMeta k "default") (fmap Just . keyString) (return Nothing) 
  pKeyType            <- ifKey (keyGetMeta k "type") (fmap parseType . keyString) (return $ Right "Top")
  pFunctionCandidates <- mapM parseFunctionCandidate =<< keyFilterMeta k (\s -> (s /= "default") && (s /= "type")) 
  pTypeSpecification  <- parseTypeSpecification k
  return $ KeySpecification pPath pDefaultValue pKeyType pFunctionCandidates pTypeSpecification
  where
    parseFunctionCandidate fk = do
      str     <- join $ liftM2 keyGetRelativeName (keyNew =<< keyString fk) prefixKey
      isArray <- arrayValidateName fk
      if isArray == Invalid
      then do
        name <- keyName fk
        return (Function name, str)
      else do
        bfk          <- flip keyAddName "#" !=<< keyDeleteBaseName !=<< keyDup fk
        baseName     <- fmap T.pack . keyGetRelativeName bfk =<< prefixKey
        let splitted = T.splitAt (T.length baseName) baseName
        return (ArrayFunction (T.unpack $ fst splitted) (T.unpack $ snd splitted), str)

parseTypeSpecification :: Key -> IO TypeSpecification
parseTypeSpecification k = do
  pName <- specPrefixKey >>= keyGetRelativeName k
  pVar  <- parsePathVariable <$> keyString k
  keyListMeta k >>= mapM_ print
  pType <- ifKey (keyGetMeta k "specElektra/type") (fmap parseTypeParameters . keyString) (return [])
  pImpl <- let parseImpl = fmap (Just . map T.unpack . T.splitOn (T.pack "\\n") . T.pack) . keyString in
           ifKey (keyGetMeta k "specElektra/impl") parseImpl (return Nothing)
  return $ TypeSpecification pName pVar pType pImpl
  where
    parseTypeParameters      = fmap (translate . splitOn "::") . splitOn "->"
    parsePathVariable "self" = Self
    parsePathVariable p      = Path p
    -- TODO use text natively is probably much better on the long run... but for now keep it simple
    translate [x,y]          = TypeParameter (parsePathVariable $ dt T.strip x) (dt T.strip y)
    translate x              = error $ "invalid translation" ++ show x

parseType :: String -> Either Path String
parseType t
  | "./" `isPrefixOf` t || "/" `isPrefixOf` t = Left t
  | otherwise = Right t

parseSpecifications :: KeySet -> (Key -> IO a) -> (KeySet -> IO KeySet) -> IO [a]
parseSpecifications ks parser keySelector = do
  parent <- keyNew "spec/examples"
  keySelector ks >>= ksList >>= mapM parser

parseKeySpecifications :: KeySet -> IO [KeySpecification]
parseKeySpecifications ks = parseSpecifications ks parseKeySpecification (\ks -> cutSpecElektra ks >> return ks)

parseTypeSpecifications :: KeySet -> IO [TypeSpecification]
parseTypeSpecifications ks = parseSpecifications ks parseTypeSpecification cutSpecElektra

cutSpecElektra :: KeySet -> IO KeySet
cutSpecElektra ks = specPrefixKey >>= ksCut ks

dt :: (T.Text -> T.Text) -> String -> String
dt fn = T.unpack . fn . T.pack

(!=<<) :: Monad m => (a -> m b) -> m a -> m a
a !=<< b = b >>= (\b' -> do _ <- a b'; return b')
infixr 1 !=<<
