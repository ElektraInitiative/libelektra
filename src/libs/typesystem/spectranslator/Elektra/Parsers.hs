--
-- @file
--
-- @brief Some parsers which are used in the translation process
--
-- To parse the type signatures which are used to carry all the
-- required information to create a corresponding specification
-- in our haskell EDSL we use a parser. As those type signatures
-- are not necessarily regular we use a parser instead of regexes
-- or string splitting to have a robust extensible foundation.
--
-- @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
--
module Elektra.Parsers (parseTypeSignature, parseRange, 
  regexTypeP, regexTypeParameterP, typeSignatureP, rangeP) where

import Control.Applicative (empty)
import Data.Void

import Elektra.Specifications

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

-- Parse the given strings without further error analysis for now

parseTypeSignature :: String -> Maybe TypeSignature
parseTypeSignature = parseMaybe typeSignatureP

parseRange :: String -> Maybe (Int, Int)
parseRange = parseMaybe rangeP

-- Lexer for the signatures

sc :: Parser ()
sc = L.space space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbolL :: String -> Parser String
symbolL = L.symbol sc

arrL :: Parser String
arrL = symbolL "->"

commaL :: Parser String
commaL = symbolL ","

cstrtL :: Parser String
cstrtL = symbolL "=>"

selfL :: Parser String
selfL = symbolL "self"

pathVarL :: Parser String
pathVarL = symbolL "::"

parensL :: Parser a -> Parser a
parensL = between (symbolL "(") (symbolL ")")

identifierL :: Parser String
identifierL = lexeme $ qual <|> unqual 
  where
    qual   = try $ (++) <$> ((++) <$> unqual <*> symbolL ".") <*> unqual
    unqual = (:) <$> letterChar <*> many alphaNumChar

pathL :: Parser String
pathL = lexeme $ many (try alphaNumChar <|> try punctuationChar) <* notFollowedBy arrL

-- Read string so everything between two "", dealing with escaped quotes as well
escapeL = choice (zipWith decode "bnfrt\\\"/" "\b\n\f\r\t\\\"/")
    where decode c r = r <$ char c

regexL :: Parser String
regexL = between (char '\"') (char '\"') (many chars)
    where chars = char '\\' *> escapeL <|> satisfy (`notElem` ("\"\\" :: String))

integerL :: Parser Int
integerL       = lexeme L.decimal

signedIntegerL :: Parser Int
signedIntegerL = L.signed sc integerL

-- Parser for the signatures

typeSignatureP :: Parser TypeSignature
typeSignatureP = between sc eof typeSigP

typeSigP :: Parser TypeSignature
typeSigP = try c <|> TypeSignature [] <$> t
  where
    c = TypeSignature <$> sepEndBy1 regexConstraintP commaL <* cstrtL <*> t
    t = sepBy1 regexTypeParameterP arrL

regexConstraintP :: Parser RegexConstraint
regexConstraintP = RegexConstraint <$> identifierL <*> regexTypeP

regexTypeP :: Parser RegexType
regexTypeP = foldl1 RegexTypeApp <$> sepEndBy1 r sc
  where
    r = try p <|> s <|> v
    p = parensL regexTypeP
    s = Regex <$> regexL
    v = RegexType <$> identifierL

regexTypeParameterP :: Parser RegexTypeParam
regexTypeParameterP = RegexTypeParam <$> regexTypeP <*> pVar
  where
    pVar = try c <|> pure Self
    c = pathVarL *> a
    a = r <|> s <|> p
    s = const Self <$> selfL
    p = Path <$> pathL
    r = Range <$> (symbolL "Range" *> pathL)

-- Parser for range regexes

rangeP :: Parser (Int, Int)
rangeP = (,) <$> signedIntegerL <* symbolL "-" <*> signedIntegerL
