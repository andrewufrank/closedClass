-----------------------------------------------------------------------------
--
-- Module      :  reformat the output from parsing
--  stanford corenlp 3.9. in json format

-- all data additional to Defs0 have 1 suffix
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


token2to0 :: (NLP.POStags postag) => postag -> Token2 -> Token0 postag
-- ^ convert a token2 dataset from JSON to Token0
-- posTag phantom indicates the type of posTags to use
token2to0 posPh (Token2 {..}) = Token0 {..}
    where
        tid = TokenID0  tok_index
        tword = Wordform0 tok_word
        tlemma = Lemma0 tok_lemma
        tpos = (NLP.parseTag  tok_pos) `asTypeOf` posPh
        tposOrig = tok_pos
        tpostt = zero
        tner = parseNERtagList [tok_ner] -- when is this a list?
                        -- use the Ner2 values?
        tspeaker = parseSpeakerTagList [tok_speaker]
        tbegin = tok_characterOffsetBegin
        tend = tok_characterOffsetEnd

coref2to0 :: Coref2 -> Mention0
coref2to0 (Coref2 {..}) = Mention0 {..}
    where
        mentRep = coref_isRepresentativeMention
        mentSent = SentID0 coref_sentNum
        mentStart = TokenID0 coref_startIndex
        mentEnd = TokenID0 coref_endIndex   -- points next word
        mentHead = TokenID0 coref_headIndex
        mentText = coref_text

data Dependence1 = Dependence1 {d1type :: DepCode -- Text -- String
                        , d1orig :: Text -- the value given in the XML
                        , d1govid :: TokenID0
                        , d1depid :: TokenID0
                        , d1govGloss :: Text
                        , d1depGloss :: Text
                        }   deriving (Show, Read, Eq)

dependency2to0 :: Dependency2 -> Dependence1
dependency2to0 Dependency2 {..} = Dependence1 {..}
    where
        d1type = parseDEPtag dep_dep :: DepCode
        d1orig = dep_dep
        d1govid = TokenID0 dep_governor
        d1depid = TokenID0 dep_dependent
        d1govGloss = dep_governorGloss
        d1depGloss = dep_dependentGloss

data Sentence1 postag = Sentence1 {s1id :: SentID0
                        , s1parse :: Text  -- the parse tree
                        , s1toks :: [Token0 postag]
                        , s1deps :: Maybe [Dependence1]
                        -- should be only one or none
                        -- select (last = best) in coreNLPxml in getSentence
                        -- could be changed to parse all and select later
                        } deriving (Read, Show,  Eq)

sentence2to0 :: (NLP.POStags postag) => postag -> Sentence2 -> Sentence1 postag
sentence2to0 posPh Sentence2 {..} = Sentence1 {..}
    where
            s1id = SentID0 s_index
            s1parse = s_parse
            s1toks = map (token2to0 posPh)  s_tokens
            s1deps = case s_enhancedPlusPlusDependencies of
                Just d1 -> Just $ map dependency2to0 d1
                Nothing -> case s_enhancedDependencies of
                    Just d2 -> Just $ map  dependency2to0 d2
                    Nothing -> case s_basicDependencies of
                        Just d3 -> Just $ map dependency2to0 d3
                        Nothing -> Nothing

data Doc1 postag = Doc1 {docSents:: [Sentence1 postag]
                 , docCorefs :: Coreferences0   -- only one
                       } deriving (Read, Show,  Eq)

data Coreferences0 = Coreferences0 {coChains:: [MentionChain0]}
                deriving (Read, Show,  Eq)

coreferences2to0 :: Coreferences2 -> Coreferences0
coreferences2to0 Coreferences2{..} = Coreferences0{..}
    where
        coChains = map corefChain2to0 chains

data MentionChain0 = MentionChain0 [Mention0] deriving (Read, Show,  Eq)

corefChain2to0 :: CorefChain2 -> MentionChain0
corefChain2to0 (CorefChain2 cs) = MentionChain0 (map coref2to0 cs)

doc2to1 ::(NLP.POStags postag) => postag -> Doc2 -> Doc1 postag
doc2to1 posPh Doc2{..} = Doc1 {..}
    where
        docSents = map (sentence2to0 posPh) doc_sentences
        docCorefs = coreferences2to0 doc_corefs
                -- chains of mentions

--
