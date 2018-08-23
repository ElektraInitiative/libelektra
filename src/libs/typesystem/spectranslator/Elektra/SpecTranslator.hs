--
-- @file
--
-- @brief Translates the parsed intermediate specifications into a compilable Haskell program using our EDSL
--
-- @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
-- 
{-# LANGUAGE CPP #-}
module Elektra.SpecTranslator (KeySpecification (..), translateSpecifications) where

import Elektra.Specifications

import Data.Char     (isAlphaNum)
import Data.Map      (Map)
import Data.Maybe    (catMaybes, isJust, fromMaybe)
import Data.List     (sortBy)
import Data.Function (on)
import Unsafe.Coerce

import qualified Data.Map.Strict as M

#if MIN_VERSION_haskell_src_exts(1,18,0)
import Language.Haskell.Exts.Build
import Language.Haskell.Exts.Syntax
import Language.Haskell.Exts.Parser
#else
import Language.Haskell.Exts.Annotated.Build
import Language.Haskell.Exts.Annotated.Syntax
import Language.Haskell.Exts.Annotated.Parser
#endif
import Language.Haskell.Exts.Extension
import Language.Haskell.Exts.SrcLoc
import qualified Language.Haskell.Exts.Parser as P

type FunctionMap = Map TypeName TypeSpecification

translateSpecifications :: [TypeSpecification] -> [KeySpecification] -> Module SrcLoc
translateSpecifications ts ks = mkModule $ concatMap translateTypeSpecification fts ++ concatMap (translateKeySpecification functions) filteredKeyDefinitions
  where
    fts = filter (isJust . implementation) ts
    functions = M.fromList [(tySpecName t, t) | t <- fts]
    filteredKeyDefinitions = filter (not . null . path) ks

translateTypeSpecification :: TypeSpecification -> [Decl SrcLoc]
translateTypeSpecification t = catMaybes [typeSig, impl <$> implementation t]
  where
    typeSig       = typeSig' <$> signature t
    impl          = let repack = foldl1 (\(FunBind _ x) (FunBind _ y) -> FunBind noLoc (x ++ y))
                        parseMode = P.defaultParseMode { P.parseFilename = renamedTySpecName t, P.extensions = [EnableExtension DataKinds]}
                    in  repack . map (unsafeCoerce . P.fromParseResult . parseDeclWithMode parseMode)
    funTypes      = foldr1 (TyFun noLoc) . map convertRegexTypeParameter
    constraint [] = Nothing
    constraint c  = Just $ CxTuple noLoc (map asst c)
    asst (RegexConstraint a p)   = AppA noLoc (mkName a) [convertRegexType p]
    typeSig' (TypeSignature c p) = TypeSig noLoc [mkName . pathToDeclName $ renamedTySpecName t] $ TyForall noLoc Nothing (constraint c) (funTypes p)

convertRegexTypeParameter :: RegexTypeParam -> Type SrcLoc
convertRegexTypeParameter (RegexTypeParam r _) = convertRegexType r

convertRegexType :: RegexType -> Type SrcLoc
convertRegexType (RegexTypeApp a b) = TyApp noLoc (convertRegexType a) (convertRegexType b)
convertRegexType (Regex r) = TyPromoted noLoc (PromotedString noLoc r r)
convertRegexType (RegexType r) = TyVar noLoc (mkName r)

translateKeySpecification :: FunctionMap -> KeySpecification -> [Decl SrcLoc]
translateKeySpecification f k = [specTranslation]
  where
    rawKeyTranslation = ExpTypeSig noLoc e t
      where
        kt = ignoreEither $ keyType k
        ignoreEither (Right r) = r
        ignoreEither _         = ".*"
        e = Con noLoc key
        t = TyCon noLoc regex <-> TyPromoted noLoc (PromotedString noLoc kt kt)
    specTranslation   = let specs  = functionCandidates k
                            conv (t, v) = foldl (<=>) (Var noLoc (translateUnqualPath $ renamedTySpecName t)) . catMaybes $ [translateFunctionParameter v]
                            sigs   = map (flip M.lookup f . functionBaseName . fncFun) specs
                            repack Nothing  _ = Nothing
                            repack (Just a) b = Just (a, b)
                            transl = conv <$> sortBy (flip compare `on` (order . fst)) (catMaybes $ zipWith repack sigs specs)
                        in mkNameBind (specificationKeyName k) $ foldr (<=>) rawKeyTranslation transl
    translateFunctionParameter v = case fncPath v of
      "" -> case fncStr v of
        "" -> Nothing
        _  -> let rgx = fncStr v
                  vr  = Var   noLoc key
                  ty  = TyCon noLoc regex <-> TyPromoted noLoc (PromotedString noLoc rgx rgx)
              in  Just $ ExpTypeSig noLoc vr ty
      _  -> Just $ Var noLoc (translateUnqualPath $ fncPath v)
    translateUnqualPath = translateUnqual . pathToDeclName

mkModule :: [Decl SrcLoc] -> Module SrcLoc
mkModule = Module noLoc
  (Just $
    ModuleHead noLoc (ModuleName noLoc "TestSpecification") Nothing Nothing)
  [LanguagePragma noLoc [mkName "DataKinds", mkName "NoImplicitPrelude"]]
  [ImportDecl {importAnn = noLoc,
               importModule = ModuleName noLoc "Elektra.RegexType",
               importQualified = False, importSrc = False, importSafe = False,
               importPkg = Nothing, importAs = Nothing, importSpecs = Nothing}
  ,ImportDecl {importAnn = noLoc,
               importModule = ModuleName noLoc "GHC.TypeLits",
               importQualified = False, importSrc = False, importSafe = False,
               importPkg = Nothing, importAs = Nothing, importSpecs = Nothing}]

renamedTySpecName :: TypeSpecification -> String
renamedTySpecName ts = fromMaybe (tySpecName ts) (rename ts)

-- AST related utilities

key :: QName SrcLoc
key = translateUnqual "Key"

regex :: QName SrcLoc
regex = translateUnqual "Regex"

specificationKeyName :: KeySpecification -> Name SrcLoc
specificationKeyName = mkName . pathToDeclName . path

translateUnqual :: String -> QName SrcLoc
translateUnqual n = UnQual noLoc (mkName n)

pathToDeclName :: String -> String
pathToDeclName = filter isAlphaNum

(<=>) :: Exp SrcLoc -> Exp SrcLoc -> Exp SrcLoc
a <=> b = App noLoc a b
infixl 5 <=>

(<->) :: Type SrcLoc -> Type SrcLoc -> Type SrcLoc
a <-> b = TyApp noLoc a b
infixl 5 <->

mkName :: String -> Name SrcLoc
#if MIN_VERSION_haskell_src_exts(1,18,0)
mkName = unsafeCoerce name
#else
mkName = name noLoc
#endif

mkNameBind :: Name SrcLoc -> Exp SrcLoc -> Decl SrcLoc
#if MIN_VERSION_haskell_src_exts(1,18,0)
mkNameBind = unsafeCoerce nameBind
#else
mkNameBind = nameBind noLoc
#endif
