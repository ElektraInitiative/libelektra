module RegexTypes.Range (regexesForRange, regexForRange) where

import Data.List (intercalate, notElem, partition, sort, nub)

-- quick port from python https://github.com/dimka665/range-regex
-- so we can easily create regexes automatically for the check/range plugin
regexForRange :: Int -> Int -> String
regexForRange l h = intercalate "|" $ regexesForRange l h

regexesForRange :: Int -> Int -> [String]
regexesForRange l h 
  | l < 0 && h < 0 = splitToPatterns (abs h) (abs l)
  | l < 0          = let n  = splitToPatterns 1 (abs l)
                         p  = splitToPatterns 0 h
                         no = ["-" ++ v  | v <- n, v `notElem` p]
                         po = [v         | v <- p, v `notElem` n]
                         i  = ["-?" ++ v | v <- n, v `elem` p]
                     in no ++ i ++ po
  | otherwise      = splitToPatterns l h

splitToPatterns :: Int -> Int -> [String]
splitToPatterns l h = let rs = splitToRanges l h
                          r  = zip (l : (map (+1) rs)) rs
                      in map (uncurry rangeToPattern) r

splitToRanges :: Int -> Int -> [Int]
splitToRanges l h = sort . nub $ h : ns ++ zs
  where
    ns = takeWhile (\s -> l <= s && s <  h) $ map (fillByNines l) [1..]
    zs = takeWhile (\s -> l < s  && s <= h) $ map (subtract 1 . fillByZeros (h + 1)) [1..]

fillByNines :: Int -> Int -> Int
fillByNines i c = let is = show i in read $ take (length is - c - 1) is ++ replicate c '9'

fillByZeros :: Int -> Int -> Int
fillByZeros i c = i - i `mod` 10 ^ c

data Digit = D String | Any deriving Show
rangeToPattern :: Int -> Int -> String 
rangeToPattern l h = concatMap (\(D c) -> c) d ++ concatMap (\_ -> "[0-9]") a
  where
    pattern   = [toPattern ld hd | (ld, hd) <- zip (show l) (show h)]
    (a, d)    = partition isAny pattern
    isAny Any = True
    isAny _   = False
    toPattern ld hd
      | ld == hd               = D [ld]
      | ld /= '0' || hd /= '9' = let (lb, hb) = if ld > hd then (hd, ld) else (ld, hd) in D ('[' : lb : '-' : hb : "]")
      | otherwise              = Any
