module Regexp where

import Prelude hiding (seq)

data Regexp = Empty
            | Epsilon
            | Char Char
            | Seq Regexp Regexp
            | Alt Regexp Regexp
            | Star Regexp
            deriving (Eq, Ord)

match :: Regexp -> String -> Bool
match r s = nullable (foldl (flip derivative) r s)

derivative :: Char -> Regexp -> Regexp
derivative ch Empty                  = Empty
derivative ch Epsilon                = Empty
derivative ch (Char ch') | ch == ch' = Epsilon
                         | otherwise = Empty
derivative ch (Seq l r) | nullable l = Alt (Seq (derivative ch l) r) (derivative ch r)
                        | otherwise  = Seq (derivative ch l) r
derivative ch (Alt l r)              = Alt (derivative ch l) (derivative ch r)
derivative ch (Star rgx)             = Seq (derivative ch rgx) (Star rgx)

nullable :: Regexp -> Bool
nullable Empty     = False
nullable Epsilon   = True
nullable (Char _)  = False
nullable (Seq l r) = nullable l && nullable r
nullable (Alt l r) = nullable l || nullable r
nullable (Star _)  = True
