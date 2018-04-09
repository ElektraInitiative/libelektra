{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
module RegexTypes.RegExpParser ( parseRegExp, parseAndNormalizeRegExp, matches, normalize, CharClass ) where

import Prelude hiding ((<$>), (<*>))
import Data.Char
import Language.HaLex.Parser
import Language.HaLex.RegExp hiding (matchesRE)

import qualified Data.Set.Range as R

type CharClass = R.RangeSet Char

-- TODO put this into own module
-- TODO ... lgpl? but i derived it quite a lot already
-- | Test whether a match can be found for the given regular expression
--   in the given sequence of characters. The regular expression is
--   allowed to contain 'OneOrMore' or 'Optional'.

matches :: RegExp CharClass -> String -> Bool
matches = matchesRE . extREtoRE

matchesRE :: RegExp CharClass -> String -> Bool
matchesRE Empty _            = False
matchesRE Epsilon inp        = null inp
matchesRE (Literal l) [x]    = x `R.queryPoint` l
matchesRE (Literal _) _      = False
matchesRE (Or re1 re2) inp   = matchesRE re1 inp || matchesRE re2 inp 
matchesRE (Then re1 re2) inp = or [ matchesRE re1 s1 && matchesRE re2 s2 
                                  | (s1,s2) <- splits inp]
matchesRE (Star re) inp      = matchesRE Epsilon inp ||
                               or [ matchesRE re s1 && matchesRE (Star re) s2 
                                  | (s1,s2) <- frontSplits inp ]
matchesRE _ _                = error "Only to be used with normalized REs"

parseAndNormalizeRegExp :: String -> Maybe (RegExp Char)
parseAndNormalizeRegExp = fmap (normalize . extREtoRE) . parseRegExp

normalize :: RegExp CharClass -> RegExp Char
normalize (Literal l)          = foldl1 Or . map Literal $ R.toList l
normalize (Or re1 re2)         = Or (normalize re1) (normalize re2)
normalize (Then re1 re2)       = Then (normalize re1) (normalize re2)
normalize (Star re)            = Star (normalize re)
normalize Epsilon = Epsilon
normalize Empty = Empty
normalize _ = error "Only to be used with normalized REs"

-- | Produce a list of all possible ways of splitting the input list
--   into two parts. For instance, 
-- @ 
--   splits "foo" 
--     = [(\"\","foo"),("f","oo"),("fo","o"),("foo",\"\")] 
-- @
splits :: [a] -> [ ([a],[a]) ]
splits s = [ splitAt n s | n <- [ 0 .. length s ] ]

-- | Produce a list of all possible ways of splitting the input list
--   into two parts where the first part is non-empy. For instance, 
-- @ 
--   splits "foo" 
--     = [("f","oo"),("fo","o"),("foo",\"\")]
-- @
frontSplits :: [a] -> [ ([a],[a]) ]
frontSplits s = [ splitAt n s | n <- [ 1 .. length s ] ]

parseRegExp :: String -> Maybe (RegExp CharClass)
parseRegExp re = res
  where parsed_re = expr re
        res | null parsed_re = Nothing
            | otherwise      = Just (fst (head parsed_re))

expr :: Parser Char (RegExp CharClass)
expr =  f  <$> termThen <*> symbol '|' <*> expr
    <|> termThen
    <|> succeed Epsilon
  where f l _ = Or l

termThen :: Parser Char (RegExp CharClass)
termThen =  f  <$> term <*> termThen
        <|> term
  where f = Then

term :: Parser Char (RegExp CharClass)
term =  f  <$> factor <*> symbol '?'
    <|> g  <$> factor <*> symbol '*'
    <|> h  <$> factor <*> symbol '+'
    <|> factor
  where
     f e _ = Or   e Epsilon
     g e _ = Star e
     h e _ = Then e (Star e)

factor :: Parser Char (RegExp CharClass)
factor =  w <$> symbol '.'
      <|> f <$> letterOrDigit
      <|> g <$> symbol '\'' <*> satisfy (const True) <*> symbol '\''
      <|> h <$> symbol '('  <*> expr                 <*> symbol ')'
      <|> k <$> symbol '['  <*> oneOrMore range      <*> symbol ']'
      <|> l <$> symbol '['  <*> symbol '^' <*> range <*> symbol ']'
  where
     w _         = Literal ascii
     f a         = Literal $ R.fromList [a]
     g _ e _     = Literal $ R.fromList [e]
     h _ e _     = e
     k _ r _     = RESet r
     l _ _ r _   = RESet [ascii `R.difference` r]

range :: Parser Char CharClass
range  =  f <$> letterOrDigit <*> symbol '-' <*> letterOrDigit
      <|> n <$> oneOrMore (satisfy (\ x -> x `R.queryPoint` ascii && x /= '-' && x /= '^'))
  where f a _ c = R.insertRange (a, c) R.empty
        n       = R.fromList
      
letterOrDigit :: Parser Char Char
letterOrDigit = satisfy (\x -> isDigit x || isAlpha x)

ascii :: CharClass
ascii =  R.insertRange ('\NUL', '\255') R.empty
