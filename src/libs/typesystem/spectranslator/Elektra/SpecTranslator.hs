--
-- @file
--
-- @brief Translates the parsed intermediate specifications into a compilable Haskell program using our EDSL
--
-- @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
-- 
module Elektra.SpecTranslator (KeySpecification (..), translateSpecifications) where

import Elektra.Specifications

import Data.Char           (isAlphaNum, toUpper)
import Data.Map            (Map)
import Data.Maybe          (catMaybes, isJust)
import Control.Monad       ((<=<))
import Unsafe.Coerce

import Language.Haskell.Exts.Build
import Language.Haskell.Exts.Syntax
import Language.Haskell.Exts.Parser
import Language.Haskell.Exts.Extension

import qualified Data.Map.Strict as M

type FunctionMap = Map TypeName TypeSpecification

translateSpecifications :: [TypeSpecification] -> [KeySpecification] -> Module ()
translateSpecifications ts ks = mkModule $ concatMap translateTypeSpecification fts ++ concatMap (translateKeySpecification functions) filteredKeyDefinitions
  where
    fts = filter (isJust . implementation) ts
    functions = M.fromList [(tySpecName t, t) | t <- fts]
    filteredKeyDefinitions = filter (not . null . path) ks

translateTypeSpecification :: TypeSpecification -> [Decl ()]
translateTypeSpecification t = catMaybes [typeSig, impl <$> implementation t]
  where
    typeSig       = typeSig' <$> signature t
    impl          = let repack = foldl1 (\(FunBind () x) (FunBind _ y) -> FunBind () (x ++ y))
                        parseMode = defaultParseMode { parseFilename = tySpecName t, extensions = [EnableExtension DataKinds]}
                    in  repack . map (unsafeCoerce . fromParseResult . parseDeclWithMode parseMode)
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
translateKeySpecification f k = [rawKeyTranslation, specTranslation]
  where
    rawKeyTranslation = nameBind (name $ rawKeyName k) $ ExpTypeSig () e t
      where
        kt = ignoreEither $ keyType k
        ignoreEither (Right r) = r
        ignoreEither _         = ".*"
        e  = Con () (translateUnqual "Key")
        t  = TyCon () (translateUnqual "Key") <-> TyPromoted () (PromotedString () kt kt)
    -- TODO default value by fallback
    specTranslation   = let specs  = functionCandidates k
                            parFld n = foldl (<=>) $ Var () (translateUnqual . pathToDeclName $ functionBaseName n)
                            conv (TypeSignature _ r, v) = parFld (fncFun v) . catMaybes $ translateFunctionParameters r (path k) v
                            sigs   = map (signature <=< flip M.lookup f . functionBaseName . fncFun) specs
                            zipped = zip sigs specs
                            repack (Nothing, _) = Nothing
                            repack (Just a , b) = Just (a, b)
                            transl = fmap conv <$> map repack zipped
                        in nameBind (specificationKeyName k) $ foldr (<=>) (Var () (translateUnqual $ rawKeyName k)) $ catMaybes transl
    translateFunctionParameters [r,_]  n v = [translateFunctionParameter r n v]
    translateFunctionParameters (r:rs) n v = translateFunctionParameter r n v : translateFunctionParameters rs n v
    translateFunctionParameters _      _ _ = error "a function must at least take a param and a return value"
    translateFunctionParameter (RegexTypeParam _ (Dispatched _)) _ v = 
      let rgx = fncStr v
          vr  = Var   () (translateUnqual "Key")
          ty  = TyCon () (translateUnqual "Key") <-> TyPromoted () (PromotedString () rgx rgx)
      in  Just $ ExpTypeSig () vr ty
    translateFunctionParameter (RegexTypeParam _ (Path  _)) _ v = Just $ Var () (translateUnqualPath $ fncPath v)
    translateFunctionParameter (RegexTypeParam _ Self     ) _ _ = Nothing
    translateUnqualPath = translateUnqual . pathToDeclName

mkModule :: [Decl ()] -> Module ()
mkModule = Module ()
  (Just $
    ModuleHead () (ModuleName () "TestSpecification") Nothing Nothing)
  [LanguagePragma () [name "TypeInType", name "NoImplicitPrelude"]]
  [ImportDecl {importAnn = (),
               importModule = ModuleName () "Elektra.RegexType",
               importQualified = False, importSrc = False, importSafe = False,
               importPkg = Nothing, importAs = Nothing, importSpecs = Nothing}
  ,ImportDecl {importAnn = (),
               importModule = ModuleName () "GHC.TypeLits",
               importQualified = False, importSrc = False, importSafe = False,
               importPkg = Nothing, importAs = Nothing, importSpecs = Nothing}]

-- AST related utilities

rawKeyName :: KeySpecification -> String
rawKeyName = ("specElektraRawKey" ++) . capitalized . pathToDeclName . path

specificationKeyName :: KeySpecification -> Name ()
specificationKeyName = name . pathToDeclName . path

translateUnqual :: String -> QName ()
translateUnqual n = UnQual () (name n)

pathToDeclName :: String -> String
pathToDeclName = filter isAlphaNum

(<=>) :: Exp () -> Exp () -> Exp ()
a <=> b = App () a b
infixl 5 <=>

(<->) :: Type () -> Type () -> Type ()
a <-> b = TyApp () a b
infixl 5 <->

-- Utilities

capitalized :: String -> String
capitalized (x:xs) = toUpper x : xs
capitalized [] = []
