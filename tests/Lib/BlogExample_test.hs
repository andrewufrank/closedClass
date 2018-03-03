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
module Lib.BlogExample_test  -- (openMain, htf_thisModuelsTests)
     where


import           Test.Framework
import           Uniform.Strings
import Uniform.FileIO
--import qualified Data.ByteString.Lazy as B

import Lib.BlogExample




-- show produces the "xx"
test_1 = do
    res0 <- runErr $ do
        let fn = makeRelFile "blog.json"
        putIOwords ["blog json decode:", showT fn]
        f <- readFile2  fn
        putIOwords ["json input:", take' 100 . showT $ f]
        let r = decodeFlickrResponse f  -- :: Maybe [Doc1]
        putIOwords ["decoded:", showT r]
        return r
    assertEqual res (show res0)

res =  "Right (Just [FlickrResponse {f_photoset = Photoset {set_id = \"72157694654577774\", set_primary = \"9774229501\", set_owner = \"101926234@N03\", set_ownername = \"mayer_tom\", set_photo = [Photo {photo_id = \"9771234001\", photo_secret = \"82sd0454a2\", photo_server = \"7348\", photo_farm = 7, photo_title = \"TESTConf test\", photo_isprimary = \"0\", photo_ispublic = 1, photo_isfriend = 0, photo_isfamily = 0}], set_page = 1, set_per_page = 500, set_perpage = 500, set_pages = 1, set_total = \"34\", set_title = \"TESTConf test\"}, f_stat = \"ok\"}])"



test_2 = assertEqual 6 6


