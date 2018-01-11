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

module Elektra.SpecTranslator (
  TransformationSpecification (..), KeySpecification (..),
  translateSpecifications
) where

import Elektra.SpecParser
import Data.Char           (isSpace, isAlphaNum, toUpper, isUpper)
import Data.List           (nub, last, (\\), union)
import Control.Applicative (liftA2)
import Unsafe.Coerce
import Data.Map            (Map)

import qualified Data.Map.Strict as M
import qualified Data.Text as T

import Language.Haskell.Exts.Build
import Language.Haskell.Exts.Syntax
import Language.Haskell.Exts.Parser

type FunctionMap         = Map TypeName TypeSpecification
type FunctionExchangeMap = Map String [FunctionCandidate]

translateSpecifications :: [TypeSpecification] -> [KeySpecification] -> Module ()
translateSpecifications ts ks = mkModule $ translatedTypes ++ translatedTypeDefinitions ++ translatedKeyDefinitions
  where
    typeDefinitions = ts
    functions = M.fromList [(typeName t, t) | t <- typeDefinitions]
    resolvedFunctions = let cef = foldl collectExchangedFunctions ([], M.empty) $ map (extract functions) ks in
      map (\k -> k { 
        functionCandidates = functionCandidates k `union` M.findWithDefault [] (path k) (snd cef) 
      }) $ fst cef
    collectExchangedFunctions (a, m) (b, n) = (b : a, m `M.union`n)
    translatedTypes           = concatMap translateType ts
    translateTypeParameters   = concatMap (translateTypeDefinitions . baseType) . typeParameters
    translatedTypeDefinitions = nub $ concatMap translateTypeParameters typeDefinitions
    translatedKeyDefinitions  = concatMap (translateKey functions) resolvedFunctions

extract :: FunctionMap -> KeySpecification -> (KeySpecification, FunctionExchangeMap)
extract fm ks = (ks { functionCandidates = functionCandidates ks \\ targetingOther }
                , M.fromListWith (++) [(snd o, [reroute o]) | o <- targetingOther])
  where
    targetingOther = filter targetsOther $ functionCandidates ks
    reroute (f, _) = (f, path ks)
    targetsOther   = maybe False (not . isSelf . typePathVariable . last . typeParameters) 
                   . (M.!?) fm . functionBaseName . fst
    isSelf Self    = True
    isSelf _       = False

mkModule :: [Decl ()] -> Module ()
mkModule = Module ()
  (Just $
    ModuleHead () (ModuleName () "TestSpecification") Nothing Nothing)
  [LanguagePragma () [name "DataKinds", name "TypeOperators", name "NoImplicitPrelude",
                      name "AllowAmbiguousTypes", name "GADTs"]]
  [ImportDecl {importAnn = (),
               importModule = ModuleName () "SpecElektra",
               importQualified = False, importSrc = False, importSafe = False,
               importPkg = Nothing, importAs = Nothing, importSpecs = Nothing}
  ,ImportDecl {importAnn = (),
               importModule = ModuleName () "Prelude",
               importQualified = True, importSrc = False, importSafe = False,
               importPkg = Nothing, importAs = Just $ ModuleName () "P", importSpecs = Nothing}
  ,ImportDecl {importAnn = (),
               importModule = ModuleName () "Unsafe.Coerce",
               importQualified = True, importSrc = False, importSafe = False,
               importPkg = Nothing, importAs = Just $ ModuleName () "U",
               importSpecs = Just $ ImportSpecList () False [IVar () $ name "unsafeCoerce" ]}]

translateType :: TypeSpecification -> [Decl ()]
translateType t = [translatedTypeSig, translatedTypeImpl]
  where
    tps = map (show . typePathVariable) $ init $ typeParameters t
    fnName = typeSpecificationName t
    translatedTypeImpl    = FunBind () [Match () (name fnName) (map (PVar () . name . outerName) tps)
                              translatedTypeSpecImpl (Just $ BDecls () [translateImpl])]
    translatedTypeSig     = let ttps = map (translateTypeParam . baseType) $ typeParameters t in
                            TypeSig () [name fnName] (foldr1 (TyFun ()) ttps)
    translateTypeParam tp = let tVar = TyVar () (name tp) in
                            TyApp () (TyCon () $ translateUnqual "Spec") tVar <->
                            TyApp () (TyCon () $ translateUnqual "Key")  tVar
    translatedTypeSpecImpl = UnGuardedRhs () $ translateApp $ translateLam translateImplApp
    translateApp lam   = let appFold tp = App () (Con () (translateUnqual "App") <=> tp) in
                         foldl appFold lam (map (Con () . translateUnqual . outerName) tps)
    translateLam impl  = let translatePattern tp = [PApp () (translateUnqual "Lft") [PVar () $ name tp]] in
                         foldr (\tp e -> Con () (translateUnqual "Lam") <=> Lambda () (translatePattern tp) e) impl tps
    translateImplApp   = Con () (translateUnqual "Lft") <=>
                         Paren () (foldl (<=>) (Var () $ translateUnqual "impl") (map (Var () . translateUnqual) tps))
    translateImpl      = maybe defaultImpl customImpl (implementation t)
      where
        resultVariable = Var () $ (translateUnqual . show . typePathVariable . last) $ typeParameters t
        defaultImpl    = let rhs = UnGuardedRhs () $ App () (Var () preludeUnsafeCoerce) resultVariable in
                         FunBind () [Match () (name "impl") (map (PVar () . name) tps) rhs Nothing]
          -- todo use some proper conversion but as we don't care about the line info unsafeCoerce is simple for now
        customImpl     = let repack = foldl1 (\(FunBind () x) (FunBind _ y) -> FunBind () (x ++ y)) in
                         repack . map (unsafeCoerce . fromParseResult . parseDecl)
    outerName (x:xs)   = 'k' : toUpper x : xs
    outerName _        = error "Cannot use empty variable names"

typeSpecificationName :: TypeSpecification -> String
typeSpecificationName = pathToDeclName . typeName

-- We can treat or datatypes as different interpretions of the given key string value
translateTypeDefinitions :: String -> [Decl ()]
translateTypeDefinitions = map (translateTypeDefinition . T.unpack . T.dropWhile (== '\''))
                         . filter (isUpper . T.head)
                         . filter (not . T.null |$| T.isPrefixOf "'")
                         . T.split (isSpace |$| (== '(') |$| (== ')'))
                         . T.pack
  where
    translateTypeDefinition n = DataDecl () (NewType ()) Nothing (DHead () (name n))
      [QualConDecl () Nothing Nothing (ConDecl () (name n) [TyCon () preludeString])] []

translateKey :: FunctionMap -> KeySpecification -> [Decl ()]
translateKey fm ks = [rawKeyTypeSig, rawKeyTranslation, specTranslation]
  where
    rawKeyTypeSig     = let rawKeyType = translateKeyType ks in
                        TypeSig () [name $ rawKeyName ks] (TyCon () (translateUnqual "Spec") <-> 
                        rawKeyType <-> (TyCon () (translateUnqual "Key") <-> rawKeyType))
    rawKeyTranslation = let k = Con () (translateUnqual "Key") <=>
                                (strE . path) ks <=> (translateDefaultValue . defaultValue) ks
                        in nameBind (name $ rawKeyName ks) $ Con () (translateUnqual "Lft") <=> k
    specTranslation   = let specs  = filter (flip M.member fm . functionBaseName . fst) $ functionCandidates ks
                            transl = foldl translateKeyTypes (Var () (translateUnqual $ rawKeyName ks)) specs
                            unqual = Var () (translateUnqual "eval") <=> transl                                     
                        in nameBind (specificationKeyName ks) unqual
    translateKeyTypes e (ArrayFunction p _, s) = let fn = Var () (translateUnqualPath p) <=> e
                                                     k  = Con () (translateUnqual "Lft") <=> 
                                                          Var () (translateUnqualPath s)
                                                 in fn <=> k
    translateKeyTypes e (Function p, _)        = Var () (translateUnqualPath p) <=> e
    -- local utils
    translateUnqualPath = translateUnqual . pathToDeclName

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
