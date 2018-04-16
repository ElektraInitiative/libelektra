--
-- @file
--
-- @brief Translates the parsed intermediate specifications into a compilable Haskell program using our EDSL
--
-- @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
-- 
{-# LANGUAGE TypeFamilies, TypeInType, TypeOperators, 
             ExistentialQuantification, GADTs, UndecidableInstances,
             PatternGuards, OverloadedStrings #-}

module Elektra.SpecTranslator2 (
  TransformationSpecification (..), KeySpecification (..),
  translateSpecifications
) where

import Elektra.Specifications
import Elektra.Parsers
import Elektra.SpecParser
import Elektra.Range

import Data.Char           (isSpace, isAlphaNum, toUpper, isUpper)
import Data.List           (nub, last, (\\), union)
import Data.Map            (Map)
import Data.Maybe          (catMaybes, maybeToList, maybe)
import Control.Applicative (liftA2)
import Control.Monad       ((<=<), mapM)
import Unsafe.Coerce

import qualified Data.Map.Strict      as M
import qualified Data.Text            as T

import Language.Haskell.Exts.Build
import Language.Haskell.Exts.Syntax
import Language.Haskell.Exts.Parser

import Debug.Trace

type FunctionMap         = Map TypeName TypeSpecification
type FunctionExchangeMap = Map String [FunctionCandidate]

translateSpecifications :: [TypeSpecification] -> [KeySpecification] -> Module ()
translateSpecifications ts ks = mkModule $ concatMap translateTypeSpecification ts ++ concatMap (translateKeySpecification functions) filteredKeyDefinitions
  where
    functions = M.fromList [(tySpecName t, t) | t <- ts]
    filteredKeyDefinitions = filter (not . null . path) ks

translateTypeSpecification :: TypeSpecification -> [Decl ()]
translateTypeSpecification t = maybeToList typeSig ++ (maybeToList . fmap impl $ implementation t)
  where
    typeSig       = typeSig' <$> signature t
    impl          = let repack = foldl1 (\(FunBind () x) (FunBind _ y) -> FunBind () (x ++ y)) in
                  repack . map (unsafeCoerce . fromParseResult . parseDecl)
    funTypes      = foldr1 (TyFun ()) . map convertRegexTypeParameter
    constraint [] = Nothing
    constraint c  = Just $ CxTuple () (map asst c)
    asst (RegexConstraint a p)   = AppA () (name $ a) [convertRegexType p]
    typeSig' (TypeSignature c p) = TypeSig () [name . pathToDeclName $ tySpecName t] $ TyForall () Nothing (constraint c) (funTypes p)


convertRegexTypeParameter :: RegexTypeParam -> Type ()
convertRegexTypeParameter (RegexTypeParam r _) = convertRegexType r

convertRegexType :: RegexType -> Type ()
convertRegexType (RegexTypeApp a b) = TyApp () (convertRegexType a) (convertRegexType b)
convertRegexType (Regex r) = TyPromoted () (PromotedString () r r)
convertRegexType (RegexType r) = TyVar () (name $ r)

translateKeySpecification :: FunctionMap -> KeySpecification -> [Decl ()]
translateKeySpecification f k = [rawKeyTypeSig, rawKeyTranslation, specTranslation]
  where
    -- TODO how do we want the inference
    ignoreEither (Right r) = r
    ignoreEither _         = ".*"
    rawKeyType        = let kt = ignoreEither $ keyType k in TyPromoted () (PromotedString () kt kt)
    rawKeyTypeSig     = TypeSig () [name $ rawKeyName k] (TyCon () (translateUnqual "Key") <-> rawKeyType)
    rawKeyTranslation = let kv = Con () (translateUnqual "Key") <=> (translateDefaultValue . defaultValue) k
                        in nameBind (name $ rawKeyName k) kv
    specTranslation   = let specs  = functionCandidates k
                            conv (TypeSignature _ r, s) = foldl (<=>) (Var () (translateUnqual $ rawKeyName k)) $ translateFunctionParameters' r s
                            sigs = map (signature <=< (f M.!?) . functionBaseName . fst) specs
                            zipped = zip sigs (map snd specs)
                            repack (Nothing, _) = Nothing
                            repack (Just a , b) = Just (a, b)
                            transl = fmap conv <$> map repack zipped
                        in nameBind (specificationKeyName k) $ foldl (<=>) (Var () (translateUnqual $ rawKeyName k)) $ catMaybes transl
    translateFunctionParameters e (ArrayFunction p _, s) = let fn = Var () (translateUnqualPath p) <=> e
                                                               k  = Var () (translateUnqualPath s)
                                                           in  fn <=> k
    translateFunctionParameters e (Function p, _)        = Var () (translateUnqualPath p) <=> e
    translateFunctionParameters' [r,x]  s = [translateFunctionParameter r s]
    translateFunctionParameters' (r:rs) s = translateFunctionParameter r s : translateFunctionParameters' rs s
    translateFunctionParameters' _      _ = error "a function must at least take a param and a return value"
    -- TODO error handling again, and path handling
    translateFunctionParameter (RegexTypeParam r (Range x)) s = let rng = parseRange s
                                                                    rgx = maybe ".*" (uncurry regexForRange) $ rng
                                                                in  Var () (preludeProxy) <=> Lit () (String () rgx rgx)
    translateFunctionParameter (RegexTypeParam r (Path  p)) s = Var () (translateUnqualPath p)
    translateFunctionParameter (RegexTypeParam r Self)      s = Var () (translateUnqual $ rawKeyName k)
    translateUnqualPath = translateUnqual . pathToDeclName

mkModule :: [Decl ()] -> Module ()
mkModule = Module ()
  (Just $
    ModuleHead () (ModuleName () "TestSpecification") Nothing Nothing)
  [LanguagePragma () [name "DataKinds", name "TypeOperators", name "NoImplicitPrelude",
                      name "AllowAmbiguousTypes", name "GADTs"]]
  [ImportDecl {importAnn = (),
               importModule = ModuleName () "Elektra.RegexType",
               importQualified = False, importSrc = False, importSafe = False,
               importPkg = Nothing, importAs = Nothing, importSpecs = Nothing}
  ,ImportDecl {importAnn = (),
               importModule = ModuleName () "GHC.TypeLits",
               importQualified = False, importSrc = False, importSafe = False,
               importPkg = Nothing, importAs = Just $ ModuleName () "P", importSpecs = Nothing}
  ,ImportDecl {importAnn = (),
               importModule = ModuleName () "Data.Proxy",
               importQualified = False, importSrc = False, importSafe = False,
               importPkg = Nothing, importAs = Just $ ModuleName () "P", importSpecs = Nothing}
  ,ImportDecl {importAnn = (),
               importModule = ModuleName () "Prelude",
               importQualified = True, importSrc = False, importSafe = False,
               importPkg = Nothing, importAs = Just $ ModuleName () "P", importSpecs = Nothing}
  ,ImportDecl {importAnn = (),
               importModule = ModuleName () "Unsafe.Coerce",
               importQualified = True, importSrc = False, importSafe = False,
               importPkg = Nothing, importAs = Just $ ModuleName () "U",
               importSpecs = Just $ ImportSpecList () False [IVar () $ name "unsafeCoerce" ]}]

-- AST related utilities

rawKeyName :: KeySpecification -> String
rawKeyName = ("specElektraRawKey" ++) . capitalized . pathToDeclName . path

specificationKeyName :: KeySpecification -> Name ()
specificationKeyName = name . pathToDeclName . path

translateDefaultValue :: Maybe String -> Exp ()
translateDefaultValue Nothing = Con () preludeNothing
translateDefaultValue (Just dv) = Con () preludeJust <=> strE dv

translateKeyType :: KeySpecification -> Type ()
translateKeyType ks 
  | Right s <- keyType ks, s == "Top" = translatePromotedType s <-> 
                                        (translatePromotedType s <-> translatePromotedType "Bot")
  | Right s <- keyType ks = translatePromotedType s
  | otherwise = error "Transforming path types of such kind is not supported yet"

translatePromotedType :: String -> Type ()
translatePromotedType n = TyPromoted () $ PromotedCon () True (UnQual () $ name n)

translateUnqual :: String -> QName ()
translateUnqual n = UnQual () (name n)

preludeString :: QName ()
preludeString = Qual () (ModuleName () "P") (name "String")
preludeNothing :: QName ()
preludeNothing = Qual () (ModuleName () "P") (name "Nothing")
preludeJust :: QName ()
preludeJust = Qual () (ModuleName () "P") (name "Just")
preludeUnsafeCoerce :: QName ()
preludeUnsafeCoerce = Qual () (ModuleName () "U") (name "unsafeCoerce")
preludeProxy :: QName ()
preludeProxy = Qual () (ModuleName () "P") (name "Proxy")

pathToDeclName :: String -> String
pathToDeclName = filter isAlphaNum

(<=>) :: Exp () -> Exp () -> Exp ()
a <=> b = App () a b
infixl 5 <=>

(<->) :: Type () -> Type () -> Type ()
a <-> b = TyApp () a b
infixl 5 <->

-- Utilities

(|$|) :: (Applicative f) => f Bool -> f Bool -> f Bool
a |$| b = liftA2 (||) a b
infixl 8 |$|

capitalized :: String -> String
capitalized (x:xs) = toUpper x : xs
capitalized [] = []
