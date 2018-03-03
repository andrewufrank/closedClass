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
module Lib.ParseJsonCoreNLP_test  -- (openMain, htf_thisModuelsTests)
     where


import           Test.Framework
import           Uniform.Strings
import Uniform.FileIO
--import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.UTF8 as B
import Data.Aeson (eitherDecode)

import Lib.ParseJsonCoreNLP

test_corefs  = do
    res0 <- runErr $ do
        let fn = makeRelFile "short1.json"
        putIOwords ["nlp json decode:", showT fn]
        f <- readFile2  fn
        putIOwords ["json input:",showStartJson f]
        let r = eitherDecode  f  :: Either String [Doc1]
        putIOwords ["decoded:", showT r]
        runErrorFromEither r
--        return r
    assertEqual resCoref  (show res0)

resCoref = ""


showStartJson = s2t . take 100 . B.toString

runErrorFromEither :: (Show s, CharChains s) => Either s a -> ErrIO a
runErrorFromEither (Left s) = throwErrorT ["runErrorFromEither", (toText s)]
                -- (toString s) -- (toText s)
runErrorFromEither (Right a) = return a

---- show produces the "xx"
--test_1 = do
--    res0 <- runErr $ do
--        let fn = makeRelFile "short1.json"
--        putIOwords ["nlp json decode:", showT fn]
--        f <- readFile2  fn
--        putIOwords ["json input:", take' 100 . showT $ f]
--        let r = decodeDoc1 f  :: Either String [Doc1]
--        putIOwords ["decoded:", showT r]
--        return r
--    assertEqual res (show res0)
--
--res =  ""


