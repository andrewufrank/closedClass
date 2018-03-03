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

module Lib.CorefOnlyEdited -- (openMain, htf_thisModuelsTests)
     where

import           Uniform.Strings
import Uniform.FileIO

import Data.Aeson as Ae
import Data.Aeson.Types  -- for modifying the labels
import GHC.Generics
--import qualified Data.ByteString.Lazy as B
import Control.Monad (mzero)
import qualified Data.HashMap.Strict as HM
--import qualified Data.Sequence as Seq
import GHC.Exts (fromList)

decodeCoref :: LazyByteString -> Either String CorefChain1
decodeCoref = eitherDecode

--data Coreferences0 = Coreferences0 {corefs :: [CorefChain1]
--                } deriving (Read, Show,  Eq, Ord, Generic)
--
--instance FromJSON Coreferences0 where
--    parseJSON =   genericParseJSON opts   . jsonToArray --  jsonMapOp
--        where
--          opts = defaultOptions  -- { fieldLabelModifier =  ("chain" ++) }
--
------ convert fields into array
--jsonToArray :: Value -> Value
----jsonToArray = id
--jsonToArray (Object vals) = error . show $  Object $ fromList [
--        ("chain",
--            Array . fromList
--                    . fmap snd . HM.toList $ vals
--                    ) ]
--
--jsonToArray x = x

data CoreferencesEdited = CoreferencesEdited {e_corefs :: Coreferences1
                } deriving (Read, Show,  Eq, Ord, Generic)

instance FromJSON CoreferencesEdited where
    parseJSON =   genericParseJSON opts
        where
          opts = defaultOptions  { fieldLabelModifier =  drop 2 }

data Coreferences1 = Coreferences1 {chains:: [CorefChain1]
                    }
                 deriving (Read, Show,  Eq, Ord, Generic)

instance FromJSON Coreferences1 where


data CorefChain1 = CorefChain1 {chain:: [Coref1]
                    }
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
