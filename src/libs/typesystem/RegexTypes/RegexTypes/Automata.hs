module RegexTypes.Automata (intersectDfa, intersectAndConvertDfa) where

import Control.Applicative (liftA2)
import Data.List (intersect, elem)

import qualified Language.HaLex.Dfa             as H
import qualified Language.HaLex.Ndfa            as H
import qualified Language.HaLex.FaOperations    as H

intersectAndConvertDfa :: (Ord a, Eq b) => Maybe (H.Dfa a b) -> Maybe (H.Dfa a b) -> Maybe (H.Dfa [a] b)
intersectAndConvertDfa hsx hsy = H.ndfa2dfa <$> liftA2 intersectDfa hsx hsy

intersectDfa :: (Eq a, Eq b) => H.Dfa a b -> H.Dfa a b -> H.Ndfa a b
intersectDfa (H.Dfa vp qp sp zp dp) (H.Dfa vq qq sq zq dq) = H.Ndfa v' q' s' z' d'
  where
    v'           = vp `intersect` vq
    q'           = qp `intersect` qq
    s'           = [sp, sq]
    z'           = zp ++ zq
    d' _ Nothing = []
    d' q (Just sy)
      | q `elem` qp && sy `elem` vp = [dp q sy]
      | q `elem` qq && sy `elem` vq = [dq q sy]
      | otherwise                   = []
