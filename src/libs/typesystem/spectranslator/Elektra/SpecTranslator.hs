--
-- @file
--
-- @brief Translates the parsed intermediate specifications into a compilable Haskell program using our EDSL
--
-- @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
-- 
module Elektra.SpecTranslator (
  TransformationSpecification (..), KeySpecification (..),
  translateSpecifications
) where

import Elektra.Specifications
import Elektra.Parsers
import Elektra.Range

import Data.Char           (isAlphaNum, toUpper)
import Data.Map            (Map)
import Data.Maybe          (catMaybes, maybeToList, maybe, isJust)
import Control.Applicative (liftA2)
import Control.Monad       ((<=<))
import Unsafe.Coerce

import Language.Haskell.Exts.Build
import Language.Haskell.Exts.Syntax
import Language.Haskell.Exts.Parser

import qualified Data.Map.Strict as M

type FunctionMap         = Map TypeName TypeSpecification

translateSpecifications :: [TypeSpecification] -> [KeySpecification] -> Module ()
translateSpecifications ts ks = mkModule $ concatMap translateTypeSpecification fts ++ concatMap (translateKeySpecification functions) filteredKeyDefinitions
  where
    isValid t = isJust (signature t) && isJust (implementation t)
    fts = filter isValid ts
    functions = M.fromList [(tySpecName t, t) | t <- fts]
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
    asst (RegexConstraint a p)   = AppA () (name a) [convertRegexType p]
    typeSig' (TypeSignature c p) = TypeSig () [name . pathToDeclName $ tySpecName t] $ TyForall () Nothing (constraint c) (funTypes p)

convertRegexTypeParameter :: RegexTypeParam -> Type ()
convertRegexTypeParameter (RegexTypeParam r _) = convertRegexType r

convertRegexType :: RegexType -> Type ()
convertRegexType (RegexTypeApp a b) = TyApp () (convertRegexType a) (convertRegexType b)
convertRegexType (Regex r) = TyPromoted () (PromotedString () r r)
convertRegexType (RegexType r) = TyVar () (name r)

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
                            parFld n = foldl (<=>) $ Var () (translateUnqual . pathToDeclName $ functionBaseName n)
                            conv (TypeSignature _ r, v) = parFld (fncFun v) . catMaybes $ translateFunctionParameters' r (path k) v
                            sigs   = map (signature <=< flip M.lookup f . functionBaseName . fncFun) specs
                            zipped = zip sigs specs
                            repack (Nothing, _) = Nothing
                            repack (Just a , b) = Just (a, b)
                            transl = fmap conv <$> map repack zipped
                        in nameBind (specificationKeyName k) $ foldr (<=>) (Var () (translateUnqual $ rawKeyName k)) $ catMaybes transl
    translateFunctionParameters e (ArrayFunction p _, s) = let fn = Var () (translateUnqualPath p) <=> e
                                                               kp  = Var () (translateUnqualPath s)
                                                           in  fn <=> kp
    translateFunctionParameters e (Function p, _)        = Var () (translateUnqualPath p) <=> e
    translateFunctionParameters' [r,_]  n v = [translateFunctionParameter r n v]
    translateFunctionParameters' (r:rs) n v = translateFunctionParameter r n v : translateFunctionParameters' rs n v
    translateFunctionParameters' _      _ _ = error "a function must at least take a param and a return value"
    -- TODO error handling again
    translateFunctionParameter (RegexTypeParam _ (Range _)) _ v = let rng = parseRange $ fncStr v
                                                                      rgx = maybe ".*" (uncurry regexForRange) rng
                                                                      vr = Var () preludeProxy
                                                                      ty  = TyCon () preludeProxy <-> TyPromoted () (PromotedString () rgx rgx)
                                                                  in  Just $ ExpTypeSig () vr ty
    translateFunctionParameter (RegexTypeParam _ (Path  _)) _ v = Just $ Var () (translateUnqualPath $ fncPath v)
    translateFunctionParameter (RegexTypeParam _ Self     ) _ _ = Nothing
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
