-----------------------------------------------------------------------------
--
-- Module      :  parsing the output of stanford corenlp 3.9. in json format
-----------------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE OverloadedStrings     #-}

{-# LANGUAGE DeriveGeneric #-}

module Lib.ParseJsonCoreNLP -- (openMain, htf_thisModuelsTests)
     where

import           Uniform.Strings
import Uniform.FileIO

import Data.Aeson
import Data.Aeson.Types  -- for modifying the labels
import GHC.Generics
--import qualified Data.ByteString.Lazy as B
import Control.Monad (mzero)
import qualified Data.HashMap.Strict as HM

parseNLP :: ErrIO ()
parseNLP = do
    f :: LazyByteString <- readFile2 (makeRelFile "short1 .json")
    let r = decodeDoc1 f -- :: Maybe [Doc1]
    putIOwords ["decoded", showT r]
    return ()

decodeDoc1 :: LazyByteString -> Maybe Doc1
decodeDoc1 = decode

data Doc1 = Doc1 {doc_sentences::  [Sentence1]
                  , doc_corefs :: CorefChain1 -- Coreferences0 -- [CorefChain1]
                       } deriving (Read, Show,  Eq, Ord, Generic)

instance FromJSON Doc1 where
    parseJSON = genericParseJSON defaultOptions {
                fieldLabelModifier = drop 4 }

data Sentence1 = Sentence1 {s_index :: Int
                        , s_parse :: Text  -- the parse tree
                        , s_basicDependencies :: [Dependency1]
                        , s_enhancedDependencies :: [Dependency1]
                        , s_enhancedPlusPlusDependencies :: [Dependency1]
                        , s_entitymentions :: [Ner1]
                        , s_tokens :: [Token1]
                        } deriving (Read, Show,  Eq, Ord, Generic)

instance FromJSON Sentence1 where
    parseJSON = genericParseJSON defaultOptions {
                fieldLabelModifier = drop 2 }

data Dependency1 = Dependency1 {dep_dep ::  Text -- the tag
                        , dep_governor :: Int
                        , dep_governorGloss :: Text
                        , dep_dependent :: Int
                        , dep_dependentGloss :: Text
                        } deriving (Read, Show,  Eq, Ord, Generic)

instance FromJSON Dependency1 where
    parseJSON = genericParseJSON defaultOptions {
                fieldLabelModifier = drop 4 }

data Ner1 = Ner1 {ner_docTokenBegin :: Int
                , ner_docTokenEnd :: Int
                , ner_tokenBegin :: Int
                , ner_tokenEnd :: Int
                , ner_text :: Text
                , ner_characterOffsetBegin :: Int
                , ner_characterOffsetEnd :: Int
                , ner_ner :: Text -- the code
                } deriving (Read, Show,  Eq, Ord, Generic)

instance FromJSON Ner1 where
    parseJSON = genericParseJSON defaultOptions {
                fieldLabelModifier = drop 4 }

data Token1 = Token1 {tok_index :: Int
                , tok_word :: Text
                , tok_originalText :: Text
                , tok_lemma :: Text
                , tok_characterOffsetBegin :: Int
                , tok_characterOffsetEnd :: Int
                , tok_pos :: Text
                , tok_ner :: Text
                , tok_speaker :: Text
                , tok_before :: Text
                , tok_after :: Text
                } deriving (Read, Show,  Eq, Ord, Generic)

instance FromJSON Token1 where
    parseJSON = genericParseJSON defaultOptions {
                fieldLabelModifier = drop 4 }

--data Coreferences0 = Coreferences0 {corefs :: CorefChain1
--                } deriving (Read, Show,  Eq, Ord, Generic)
--
--instance FromJSON Coreferences0 where


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
