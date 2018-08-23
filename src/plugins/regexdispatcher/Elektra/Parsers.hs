--
-- @file
--
-- @brief Some parsers that are used by the regexdispatcher to parse metakeys 
-- before generating regexes out of them
--
-- @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
-- 
{-# LANGUAGE CPP #-}
module Elektra.Parsers (parseRange) where

import Control.Applicative (empty)

import Text.Megaparsec
import Text.Megaparsec.Char
#if MIN_VERSION_megaparsec(6,0,0)
import qualified Text.Megaparsec.Char.Lexer as L
#else
import qualified Text.Megaparsec.Lexer as L
#endif

#if MIN_VERSION_megaparsec(6,0,0)
import Data.Void
type Parser = Parsec Void String
#else
type Parser = Parsec Dec String
#endif

-- Parse the given strings without further error analysis for now

parseRange :: String -> Maybe (Int, Int)
parseRange = parseMaybe rangeP

-- Basic lexer definitions

sc :: Parser ()
sc = L.space (const () <$> spaceChar) empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbolL :: String -> Parser String
symbolL = L.symbol sc

integerL :: Parser Int
integerL = fromIntegral <$> lexeme L.decimal

signedIntegerL :: Parser Int
signedIntegerL = fromIntegral <$> L.signed sc integerL

-- Parser for range regexes

rangeP :: Parser (Int, Int)
rangeP = (,) <$> signedIntegerL <* symbolL "-" <*> signedIntegerL
