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

import Data.Aeson as Ae
import Data.Aeson.Types  -- for modifying the labels
import GHC.Generics
import Control.Monad (mzero)
import qualified Data.HashMap.Strict as HM
import GHC.Exts (fromList)
--import qualified Data.ByteString.Lazy as B
--import qualified Data.Sequence as Seq

decodeCoref :: LazyByteString -> Either String CorefChain1
decodeCoref = eitherDecode

data Coreferences0 = Coreferences0 {corefs :: Coreferences1
                } deriving (Read, Show,  Eq, Ord, Generic)

instance FromJSON Coreferences0 where
    parseJSON =   genericParseJSON opts
        where
          opts = defaultOptions

data Coreferences1 = Coreferences1 -- [CorefChain1]
        {chains:: [CorefChain1]                }
                 deriving (Read, Show,  Eq, Ord, Generic)

instance FromJSON Coreferences1 where
    parseJSON =   genericParseJSON opts  . jsonToArray -
        where
          opts = defaultOptions

---- convert fields into array -- applied before the parse of Coreferences1
jsonToArray :: Value -> Value
--jsonToArray = id
jsonToArray (Object vals) = -- error . show $
    object ["chains" .= (fmap snd . HM.toList $ vals) ]
jsonToArray x = x


data CorefChain1 = CorefChain1 [Coref1] -- {chain:: [Coref1]  }
                 deriving (Read, Show,  Eq, Ord, Generic)

instance FromJSON CorefChain1 where


data Coref1 = Coref1 {coref_id :: Int
                    , coref_text :: Text
--                    , coref_type :: Text
--                    , coref_number :: Text
--                    , coref_gender :: Text
--                    , coref_animacy :: Text
--                    , coref_startIndex :: Int
--                    , coref_endIndex :: Int
--                    , coref_headIndex :: Int
--                    , coref_sentNum :: Int
--                    , coref_position :: [Int]
                    , coref_isRepresentativeMention :: Bool
                } deriving (Read, Show,  Eq, Ord, Generic)

instance FromJSON Coref1 where
    parseJSON =   genericParseJSON opts
        where
          opts = defaultOptions { fieldLabelModifier =  drop 6 }

--
