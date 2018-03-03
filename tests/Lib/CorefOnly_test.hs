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
        let r = decodeCoref f  -- :: Either String CorefChain1
        putIOwords ["decoded:", showT r]
        either2error r
--        return r
    assertEqual res (show res0)

res = ""

--eitherString2Text :: Either s a -> Either Text a
--eitherString2Text (Left s) = Left (s2t s)
--eitherString2Text (Right a) = Right a

either2error :: (Show s, CharChains s) => Either s a -> ErrIO a
either2error (Left s) = throwErrorT ["either2error", (toText s)]
                -- (toString s) -- (toText s)
either2error (Right a) = return a

--runErr2 :: (Either String a) ->
--runErr2 op = case runErr op of
---- treat computations which yield Either String V
--    Left m -> Left m
--    Right (Left a) -> Left (toText a)
--    Right (Right a) -> Right a
--    Right x -> Right x

