--
-- @file
--
-- @brief Data types for the translation process
--
-- @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
-- 
module Elektra.Specifications (
  Path, Implementation, FunctionCandidate (..), TypeName, PathVariable (..), Function (..),
  TypeSpecification (..), TypeSignature (..), RegexTypeParam (..), RegexType (..),
  RegexConstraint (..), TransformationSpecification (..), KeySpecification (..), functionBaseName
) where

type Path = String
type Implementation = String
type TypeName = String

data FunctionCandidate = FunctionCandidate {
  fncFun  :: Function,
  fncPath :: Path,
  fncStr  :: String
} deriving (Show, Eq)

data Function = ArrayFunction TypeName String
              | Function TypeName
              deriving Eq

instance Show Function
  where show (ArrayFunction s i) = s ++ i
        show (Function s)        = s

functionBaseName :: Function -> String
functionBaseName (ArrayFunction s _) = s
functionBaseName (Function s)        = s

data PathVariable = Self
                  | Path Path
                  | Range Path
                  deriving Eq

instance Show PathVariable
  where
    show Self      = "self"
    show (Path p)  = "@PATH@" ++ p
    show (Range r) = "@RANGE@" ++ r

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

data TypeSignature = TypeSignature [RegexConstraint] [RegexTypeParam] deriving (Show, Eq)

data RegexTypeParam = RegexTypeParam RegexType PathVariable deriving (Show, Eq)

data RegexConstraint = RegexConstraint String RegexType deriving (Show, Eq)

data RegexType = RegexType String
               | RegexTypeApp RegexType RegexType
               | Regex String
               deriving (Show, Eq)

data TypeSpecification = TypeSpecification {
  tySpecName     :: String,
  tyPathVar      :: Maybe Path,
  signature      :: Maybe TypeSignature,
  implementation :: Maybe [Implementation]
} deriving (Show, Eq)
