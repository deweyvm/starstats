{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module StarStats.Utils where

import Text.Printf
class Print a where
    print' :: a -> String

instance Print Int where
    print' = show

instance Print Double where
    print' = printf "%.1f"

instance Print String where
    print' = id


class Default a where
    default' :: a

instance Default Double where
    default' = 0

instance Default Int where
    default' = 0

instance Default [Char] where
    default' = ""
