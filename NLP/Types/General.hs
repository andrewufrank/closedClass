{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module NLP.Types.General (module NLP.Types.General
--    , module Data.Utilities
    ) where

import Data.Serialize (Serialize)
--import Data.Text (Text)
--import qualified Data.Text as T
import GHC.Generics

import Test.QuickCheck (Arbitrary(..), elements)

import Data.Utilities

-- | Boolean type to indicate case sensitivity for textual
-- comparisons.
data CaseSensitive = Sensitive | Insensitive
  deriving (Read, Show, Generic)

instance Serialize CaseSensitive
instance Arbitrary CaseSensitive where
  arbitrary = elements [Sensitive, Insensitive]
