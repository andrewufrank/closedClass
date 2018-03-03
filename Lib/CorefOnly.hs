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
--import qualified Data.ByteString.Lazy as B
import Control.Monad (mzero)
import qualified Data.HashMap.Strict as HM
--import qualified Data.Sequence as Seq
import GHC.Exts (fromList)

decodeCoref :: LazyByteString -> Either String CorefChain1
decodeCoref = eitherDecode

data Coreferences0 = Coreferences0 {corefs :: [CorefChain1]
                } deriving (Read, Show,  Eq, Ord, Generic)

instance FromJSON Coreferences0 where


data CorefChain1 = CorefChain1 {chain :: Coref1
                 deriving (Read, Show,  Eq, Ord, Generic)

instance FromJSON CorefChain1 where
    parseJSON =   genericParseJSON opts   . jsonToArray --  jsonMapOp
        where
          opts = defaultOptions  -- { fieldLabelModifier =  ("chain" ++) }

---- convert fields into array
jsonToArray :: Value -> Value
--jsonToArray = id
jsonToArray = Array . fromList . jsonToArrayx


----jsonToArray vals =  encode . Seq.fromList . map snd . HM.toList $ vals
--jsonToArray vals =  Ae.Array . map snd . HM.toList $ vals
jsonToArrayx (Object vals) =   fmap snd . HM.toList $ (vals )  ::[Value]

-- | Turn all keys in a JSON object to "chain"
jsonMapOp :: Value -> Value
jsonMapOp (Object o) = Object . HM.fromList . map op . HM.toList $ o
  where op :: (Text,t1) -> (Text,t1)
        op (key, val) = (key, val)
--        key2 :: String -> String
--        key2 key = key -- "chain" ++ key :: String
jsonMapOp x = x

data Coref1 = Coref1 {coref_id :: Int
                    , coref_text :: Text
--                    , coref_type :: Text
--                    , coref_number :: Text
--                    , coref_gender :: Text
--                    , coref_animacy :: Text
--                    , coref_startIndex :: Int
--                    , coref_endIndex :: Int
                    , coref_headIndex :: Int
                    , coref_sentNum :: Int
--                    , coref_position :: [Int]
                    , coref_isRepresentativeMention :: Bool
                } deriving (Read, Show,  Eq, Ord, Generic)

instance FromJSON Coref1 where
    parseJSON =   genericParseJSON opts
        where
          opts = defaultOptions { fieldLabelModifier =  drop 6 }

--
