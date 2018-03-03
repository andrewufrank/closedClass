-----------------------------------------------------------------------------
--
-- Module      :   a test for HTF framework
-- insert {-@ HTF_TESTS @-} for each import
-----------------------------------------------------------------------------
{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE OverloadedStrings     #-}
module Lib.CorefOnly_test  -- (openMain, htf_thisModuelsTests)
     where


import           Test.Framework
import           Uniform.Strings
import Uniform.FileIO
--import qualified Data.ByteString.Lazy as B

import Lib.CorefOnly




-- show produces the "xx"
test_1 = do
    res0 <- runErr $ do
        let fn = makeRelFile "coreforig.json"
--             "coref2.json"
                -- coref1.json manually replaced to "chain"
        putIOwords ["nlp json decode:", showT fn]
        f <- readFile2  fn
        putIOwords ["json input:", take' 100 . showT $ f]
        let r = decodeCoref f  -- :: Maybe [Doc1]
        putIOwords ["decoded:", showT r]
        return r
    assertEqual res (show res0)

-- res =  "Right (Just (Coreferences0 {corefs = CorefChain1 {chain = [Coref1 {coref_id = 1, coref_text = \"the uncle\", coref_type = \"NOMINAL\", coref_number = \"SINGULAR\", coref_gender = \"MALE\", coref_animacy = \"ANIMATE\", coref_startIndex = 2, coref_endIndex = 4, coref_headIndex = 3, coref_sentNum = 1, coref_position = [1,1], coref_isRepresentativeMention = True},Coref1 {coref_id = 3, coref_text = \"he\", coref_type = \"PRONOMINAL\", coref_number = \"SINGULAR\", coref_gender = \"MALE\", coref_animacy = \"ANIMATE\", coref_startIndex = 9, coref_endIndex = 10, coref_headIndex = 9, coref_sentNum = 1, coref_position = [1,3], coref_isRepresentativeMention = False}]}}))"
res = "Right (Just (Coreferences0 {corefs = CorefChain1 {chain = [Coref1 {coref_id = 2, coref_text = \"the room\", coref_type = \"NOMINAL\", coref_number = \"SINGULAR\", coref_gender = \"NEUTRAL\", coref_animacy = \"INANIMATE\", coref_startIndex = 6, coref_endIndex = 8, coref_headIndex = 7, coref_sentNum = 1, coref_position = [1,2], coref_isRepresentativeMention = True},Coref1 {coref_id = 5, coref_text = \"It\", coref_type = \"PRONOMINAL\", coref_number = \"SINGULAR\", coref_gender = \"NEUTRAL\", coref_animacy = \"INANIMATE\", coref_startIndex = 1, coref_endIndex = 2, coref_headIndex = 1, coref_sentNum = 2, coref_position = [2,1], coref_isRepresentativeMention = False}]}}))"



