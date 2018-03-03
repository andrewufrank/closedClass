-----------------------------------------------------------------------------
--
-- Module      :  parsing the corefs only
-----------------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE OverloadedStrings     #-}

{-# LANGUAGE DeriveGeneric #-}

module Lib.CorefOnly -- (openMain, htf_thisModuelsTests)
     where

import           Uniform.Strings
import Uniform.FileIO

import Data.Aeson
import Data.Aeson.Types  -- for modifying the labels
import GHC.Generics
--import qualified Data.ByteString.Lazy as B
import Control.Monad (mzero)
import qualified Data.HashMap.Strict as HM


decodeCoref :: LazyByteString -> Maybe Coreferences0
decodeCoref = decode

data Coreferences0 = Coreferences0 {corefs :: CorefChain1
                } deriving (Read, Show,  Eq, Ord, Generic)

instance FromJSON Coreferences0 where


data CorefChain1 = CorefChain1 {chain :: [Coref1]
                } deriving (Read, Show,  Eq, Ord, Generic)

instance FromJSON CorefChain1 where
    parseJSON =   genericParseJSON opts . jsonLower
        where
          opts = defaultOptions -- { fieldLabelModifier =  drop 6 }

-- | Turn all keys in a JSON object to "chain"
jsonLower :: Value -> Value
jsonLower (Object o) = Object . HM.fromList . map lowerPair . HM.toList $ o
  where lowerPair (key, val) = ("chain", val)
jsonLower x = x

data Coref1 = Coref1 {coref_id :: Int
                    , coref_text :: Text
                    , coref_type :: Text
                    , coref_number :: Text
                    , coref_gender :: Text
                    , coref_animacy :: Text
                    , coref_startIndex :: Int
                    , coref_endIndex :: Int
                    , coref_headIndex :: Int
                    , coref_sentNum :: Int
                    , coref_position :: [Int]
                    , coref_isRepresentativeMention :: Bool
                } deriving (Read, Show,  Eq, Ord, Generic)

instance FromJSON Coref1 where
    parseJSON =   genericParseJSON opts
        where
          opts = defaultOptions { fieldLabelModifier =  drop 6 }

--
