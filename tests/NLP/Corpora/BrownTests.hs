{-# OPTIONS_GHC -F -pgmF htfpp #-}

module NLP.Corpora.BrownTests where

import Test.Framework

import qualified NLP.Corpora.Brown as B
-- qualification here is not required

import  NLP.Types.Tags

--import NLP.Types

--tests :: TestTree
--tests = testGroup "NLP.Corpora.Brown"
--        [ testProperty "Brown POS Tags round-trip" prop_tagsRoundTrip
--        ]

prop_tagsRoundTrip :: B.POStag -> Bool
prop_tagsRoundTrip tag = tag == (parseTag . fromTag) tag

