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
import qualified Data.ByteString.Lazy.UTF8 as B
import Data.Aeson (eitherDecode)

import Lib.CorefOnly


test_coref = do
    res0 <- runErr $ do
        let fn = makeRelFile "coref.json"
        putIOwords ["nlp json decode:", showT fn]
        f <- readFile2  fn
        putIOwords ["json input:", take' 100 . showT $ f]
        let r = eitherDecode  f  :: Either String Coref1
        putIOwords ["decoded:", showT r]
        runErrorFromEither r
--        return r
    assertEqual resCoref (show res0)

resCoref =  "Right (Coref1 {coref_id = 3, coref_text = \"he\", coref_headIndex = 9, coref_sentNum = 1, coref_isRepresentativeMention = False})"

test_chain = do
    res0 <- runErr $ do
        let fn = makeRelFile "chain.json"
        putIOwords ["nlp json decode:", showT fn]
        f <- readFile2  fn
        putIOwords ["json input:", s2t . take 100 . B.toString $ f]
        let r = eitherDecode  f  :: Either String CorefChain1
        putIOwords ["decoded:", showT r]
        runErrorFromEither r
--        return r
    assertEqual resChain (show res0)

resChain =  "Right (CorefChain1 {chain = [Coref1 {coref_id = 1, coref_text = \"the uncle\", coref_headIndex = 3, coref_sentNum = 1, coref_isRepresentativeMention = True},Coref1 {coref_id = 3, coref_text = \"he\", coref_headIndex = 9, coref_sentNum = 1, coref_isRepresentativeMention = False}]})"


---- show produces the "xx"
--test_1 = do
--    res0 <- runErr $ do
--        let fn = makeRelFile "coreforig.json"
----             "coref2.json"
--                -- coref1.json manually replaced to "chain"
--        putIOwords ["nlp json decode:", showT fn]
--        f <- readFile2  fn
--        putIOwords ["json input:", take' 100 . showT $ f]
--        let r = decodeCoref f  -- :: Either String CorefChain1
--        putIOwords ["decoded:", showT r]
--        runErrorFromEither r
----        return r
--    assertEqual res (show res0)
--
--res = ""

--eitherString2Text :: Either s a -> Either Text a
--eitherString2Text (Left s) = Left (s2t s)
--eitherString2Text (Right a) = Right a

runErrorFromEither :: (Show s, CharChains s) => Either s a -> ErrIO a
runErrorFromEither (Left s) = throwErrorT ["runErrorFromEither", (toText s)]
                -- (toString s) -- (toText s)
runErrorFromEither (Right a) = return a

--runErr2 :: (Either String a) ->
--runErr2 op = case runErr op of
---- treat computations which yield Either String V
--    Left m -> Left m
--    Right (Left a) -> Left (toText a)
--    Right (Right a) -> Right a
--    Right x -> Right x

