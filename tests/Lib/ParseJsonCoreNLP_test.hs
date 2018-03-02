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

import Lib.ParseJsonCoreNLP




-- show produces the "xx"
test_1 = do
    res0 <- runErr $ do
        f <- readFile2  (makeRelFile "short1.json")
        let r = decodeDoc1 f  -- :: Maybe [Doc1]
        putIOwords ["decoded", showT r]
        return r
    assertEqual res (show res0)

res =  ""


