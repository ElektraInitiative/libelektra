--
-- @file
--
-- @brief Translates the parsed intermediate specifications into a compilable Haskell program using our EDSL
--
-- @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
-- 
module Elektra.SpecTranslator (KeySpecification (..), translateSpecifications) where

import Elektra.Specifications

import Data.Char     (isAlphaNum)
import Data.Map      (Map)
import Data.Maybe    (catMaybes, isJust, fromMaybe)
import Data.List     (sortBy)
import Data.Function (on)
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
                        parseMode = defaultParseMode { parseFilename = renamedTySpecName t, extensions = [EnableExtension DataKinds]}
                    in  repack . map (unsafeCoerce . fromParseResult . parseDeclWithMode parseMode)
    funTypes      = foldr1 (TyFun ()) . map convertRegexTypeParameter
    constraint [] = Nothing
    constraint c  = Just $ CxTuple () (map asst c)
    asst (RegexConstraint a p)   = AppA () (name a) [convertRegexType p]
    typeSig' (TypeSignature c p) = TypeSig () [name . pathToDeclName $ renamedTySpecName t] $ TyForall () Nothing (constraint c) (funTypes p)

convertRegexTypeParameter :: RegexTypeParam -> Type ()
convertRegexTypeParameter (RegexTypeParam r _) = convertRegexType r

convertRegexType :: RegexType -> Type ()
convertRegexType (RegexTypeApp a b) = TyApp () (convertRegexType a) (convertRegexType b)
convertRegexType (Regex r) = TyPromoted () (PromotedString () r r)
convertRegexType (RegexType r) = TyVar () (name r)

translateKeySpecification :: FunctionMap -> KeySpecification -> [Decl ()]
translateKeySpecification f k = [specTranslation]
  where
    rawKeyTranslation = ExpTypeSig () e t
      where
        kt = ignoreEither $ keyType k
        ignoreEither (Right r) = r
        ignoreEither _         = ".*"
        e = Con () key
        t = TyCon () regex <-> TyPromoted () (PromotedString () kt kt)
    specTranslation   = let specs  = functionCandidates k
                            conv (t, v) = foldl (<=>) (Var () (translateUnqualPath $ renamedTySpecName t)) . catMaybes $ [translateFunctionParameter v]
                            sigs   = map (flip M.lookup f . functionBaseName . fncFun) specs
                            repack Nothing  _ = Nothing
                            repack (Just a) b = Just (a, b)
                            transl = conv <$> sortBy (flip compare `on` (order . fst)) (catMaybes $ zipWith repack sigs specs)
                        in nameBind (specificationKeyName k) $ foldr (<=>) rawKeyTranslation transl
    translateFunctionParameter v = case fncPath v of
      "" -> case fncStr v of
        "" -> Nothing
        _  -> let rgx = fncStr v
                  vr  = Var   () key
                  ty  = TyCon () regex <-> TyPromoted () (PromotedString () rgx rgx)
              in  Just $ ExpTypeSig () vr ty
      _  -> Just $ Var () (translateUnqualPath $ fncPath v)
    translateUnqualPath = translateUnqual . pathToDeclName

mkModule :: [Decl ()] -> Module ()
mkModule = Module ()
  (Just $
    ModuleHead () (ModuleName () "TestSpecification") Nothing Nothing)
  [LanguagePragma () [name "DataKinds", name "NoImplicitPrelude"]]
  [ImportDecl {importAnn = (),
               importModule = ModuleName () "Elektra.RegexType",
               importQualified = False, importSrc = False, importSafe = False,
               importPkg = Nothing, importAs = Nothing, importSpecs = Nothing}
  ,ImportDecl {importAnn = (),
               importModule = ModuleName () "GHC.TypeLits",
               importQualified = False, importSrc = False, importSafe = False,
               importPkg = Nothing, importAs = Nothing, importSpecs = Nothing}]

renamedTySpecName :: TypeSpecification -> String
renamedTySpecName ts = fromMaybe (tySpecName ts) (rename ts)

-- AST related utilities

key :: QName ()
key = translateUnqual "Key"

regex :: QName ()
regex = translateUnqual "Regex"

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
