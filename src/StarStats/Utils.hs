{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module StarStats.Utils(
    Print(..),
    Default(..)
) where

import Text.Printf
class Print a where
    print' :: a -> String

instance Print Int where
    print' = show

instance Print Double where
    print' = printf "%.1f"

instance Print String where
    print' = escapeHtml


class Default a where
    default' :: a

instance Default Double where
    default' = 0

instance Default Int where
    default' = 0

instance Default [Char] where
    default' = ""

replaceChar :: Char -> String -> String -> String
replaceChar c r s =
    concat $ helper c s r
    where helper :: Char -> String -> String -> [String]
          helper c (x:xs) r =
              if c == x
              then r : helper c xs r
              else [x] : helper c xs r
          helper c [] r = [[]]

escapeHtml :: String -> String
escapeHtml =
    replaceChar '>' "&gt;" . replaceChar '<' "&lt;"
