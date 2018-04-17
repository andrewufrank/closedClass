-----------------------------------------------------------------------------
--
-- Module      :   a test for HTF framework
-- insert {-@ HTF_TESTS @-} for each import
-----------------------------------------------------------------------------
--{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE OverloadedStrings     #-}

module FirstSub
    where

import qualified Data.Text as T
import  Data.Text (Text)
import qualified Data.Text.IO as T

import Control.Monad.Error  -- is monads-tf
import NLP.POS.AvgPerceptronTagger
--import NLP.POS.LiteralTagger
import NLP.Types.Tags
import NLP.Tokenize.Chatter (tokenize)
import NLP.Types.Tree (Sentence, TaggedSentence)
import NLP.Types.Tags (parseTag)
import NLP.ML.AvgPerceptron (Perceptron)
import NLP.Corpora.Conll as Conll

firstSub :: IO ()
firstSub = do
    putIOwords ["firstSub start", "trainingSentence:", trainingSentences]
    t1 :: Perceptron <- trainNew parseTagConnl trainingSentences
--    putIOwords ["use tagger ", "for sentence:", testSentence]
    let r1 = tag t1 $  testSentTokenized :: [TaggedSentence Conll.Tag]
--    putIOwords ["result", T.pack $ show r1]
    return ()

parseTagConnl :: Text -> Conll.Tag
parseTagConnl = parseTag

trainingSentences = "The/DT dog/NN jumped/VB ./.\nThe/DT cat/NN slept/VB ./." :: Text
testSentence = T.lines $ "The doc slept." :: [Text]
testSentTokenized = map tokenize testSentence :: [Sentence]

putIOwords ::  [Text] -> IO()
putIOwords = T.putStrLn . T.unwords

