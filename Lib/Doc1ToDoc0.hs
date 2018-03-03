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
{-# LANGUAGE OverloadedStrings
    , RecordWildCards     #-}

--{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

--{-# LANGUAGE TemplateHaskell #-}
-- template haskell requires reordering of data types
--and all functions used for default otions imported (not local defined)

module Lib.Doc1ToDoc0 -- (openMain, htf_thisModuelsTests)
     where

import           Uniform.Strings
--import Uniform.FileIO
import CoreNLP.Defs0
import Lib.ParseJsonCoreNLP
import qualified NLP.Types.Tags      as NLP
--import qualified NLP.Corpora.Conll  as Conll
--import qualified NLP.Corpora.UD  as UD
--            Uniform.FileIO
--import              LitNLP.Tools
import              CoreNLP.DEPcodes
import              CoreNLP.NERcodes
import Uniform.Zero
--parseNLP :: ErrIO ()


token2to0 :: (NLP.POStags postag) => postag -> Token1 -> Token0 postag
-- ^ convert a token2 dataset from JSON to Token0
-- posTag phantom indicates the type of posTags to use
token2to0 posPh (Token1 {..}) = Token0 {..}
    where
        tid = TokenID0  tok_index
        tword = Wordform0 tok_word
        tlemma = Lemma0 tok_lemma
        tpos = (NLP.parseTag  tok_pos) `asTypeOf` posPh
        tposOrig = tok_pos
        tpostt = zero
        tner = parseNERtagList [tok_ner] -- when is this a list?
                        -- use the Ner1 values?
        tspeaker = parseSpeakerTagList [tok_speaker]
        tbegin = tok_characterOffsetBegin
        tend = tok_characterOffsetEnd

coref1to0 :: Coref1 -> Mention0
coref1to0 (Coref1 {..}) = Mention0 {..}
    where
        mentRep = coref_isRepresentativeMention
        mentSent = SentID0 coref_sentNum
        mentStart = TokenID0 coref_startIndex
        mentEnd = TokenID0 coref_endIndex   -- points next word
        mentHead = TokenID0 coref_headIndex
        mentText = coref_text

data Dependence01 = Dependence01 {d1type :: DepCode -- Text -- String
                        , d1orig :: Text -- the value given in the XML
                        , d1govid :: TokenID0
                        , d1depid :: TokenID0
                        , d1govGloss :: Text
                        , d1depGloss :: Text
                        }   deriving (Show, Read, Eq)

dependency1to0 :: Dependency1 -> Dependence01
dependency1to0 Dependency1 {..} = Dependence01 {..}
    where
        d1type = parseDEPtag dep_dep :: DepCode
        d1orig = dep_dep
        d1govid = TokenID0 dep_governor
        d1depid = TokenID0 dep_dependent
        d1govGloss = dep_governorGloss
        d1depGloss = dep_dependentGloss

data Sentence01 postag = Sentence01 {s1id :: SentID0
                        , s1parse :: Text  -- the parse tree
                        , s1toks :: [Token0 postag]
                        , s1deps :: Maybe [Dependence01]
                        -- should be only one or none
                        -- select (last = best) in coreNLPxml in getSentence
                        -- could be changed to parse all and select later
                        } deriving (Read, Show,  Eq)

sentence1to0 :: (NLP.POStags postag) => postag -> Sentence1 -> Sentence01 postag
sentence1to0 posPh Sentence1 {..} = Sentence01 {..}
    where
            s1id = SentID0 s_index
            s1parse = s_parse
            s1toks = map (token2to0 posPh)  s_tokens
            s1deps = case s_enhancedPlusPlusDependencies of
                Just d1 -> Just $ map dependency1to0 d1
                Nothing -> case s_enhancedDependencies of
                    Just d2 -> Just $ map  dependency1to0 d2
                    Nothing -> case s_basicDependencies of
                        Just d3 -> Just $ map dependency1to0 d3
                        Nothing -> Nothing

data Doc01 postag = Doc01 {docSents:: [Sentence01 postag]
                 , docCorefs :: Coreferences0   -- only one
                       } deriving (Read, Show,  Eq)

data Coreferences0 = Coreferences0 {coChains:: [MentionChain0]}
                deriving (Read, Show,  Eq)

coreferences1to0 :: Coreferences1 -> Coreferences0
coreferences1to0 Coreferences1{..} = Coreferences0{..}
    where
        coChains = map corefChain1to0 chains

data MentionChain0 = MentionChain0 [Mention0] deriving (Read, Show,  Eq)

corefChain1to0 :: CorefChain1 -> MentionChain0
corefChain1to0 (CorefChain1 cs) = MentionChain0 (map coref1to0 cs)

doc1to01 ::(NLP.POStags postag) => postag -> Doc1 -> Doc01 postag
doc1to01 posPh Doc1{..} = Doc01 {..}
    where
        docSents = map (sentence1to0 posPh) doc_sentences
        docCorefs = coreferences1to0 doc_corefs
                -- chains of mentions

--
