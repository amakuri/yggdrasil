module Prelude
    ( module ClassyPrelude
    , module System.FilePath
    , module Data.Void
    -- , module Control.Arrow
    -- , head
    -- , last
    -- , init
    -- , orElse
    , (->>)
    ) where

import ClassyPrelude hiding (head, last, init, try, catchIO)
-- import Foundation
-- import Prelude
import System.FilePath
import Data.Void
-- import Control.Arrow hiding (first, second)

-- head :: [a] -> Maybe a
-- head []    = Nothing
-- head (x:_) = Just x

-- last :: [a] -> Maybe a
-- last []     = Nothing
-- last (x:[]) = Just x
-- last (_:xs) = last xs

-- init :: [a] -> Maybe [a]
-- init [] = Nothing
-- init (x:[]) = Just []
-- init (x:xs) = (x:) <$> init xs

-- orElse :: Maybe a -> b -> Either b a
-- orElse (Just a) _ = Right a
-- orElse Nothing b = Left b


-- (++) :: FilePath -> FilePath -> FilePath
-- (++) = (<>)

(->>) :: a -> (a -> b) -> b
a->>f = f a

infixl 9 ->>
