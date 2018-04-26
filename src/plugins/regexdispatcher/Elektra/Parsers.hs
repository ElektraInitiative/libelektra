--
-- @file
--
-- @brief Some parsers which are used in the translation process
--
-- @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
-- 
module Elektra.Parsers (parseRange) where

import Control.Applicative (empty)
import Data.Void

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

-- Accessory functions

parseRange :: String -> Maybe (Int, Int)
parseRange = parseMaybe rangeP

-- Lexer for the signatures, primitive string operations are just wrong to use for that...

sc :: Parser ()
sc = L.space space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbolL :: String -> Parser String
symbolL = L.symbol sc

integerL :: Parser Int
integerL = lexeme L.decimal

signedIntegerL :: Parser Int
signedIntegerL = L.signed sc integerL

-- Parser for range regexes

rangeP :: Parser (Int, Int)
rangeP = (,) <$> signedIntegerL <* symbolL "-" <*> signedIntegerL
