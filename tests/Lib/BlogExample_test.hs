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
import qualified Data.ByteString.Lazy as B

import Lib.BlogExample




-- show produces the "xx"
test_1 = do
    f <- readBSlazy  "blog.json"
    let r = decodeFlickrResponse f  -- :: Maybe [FlickrResponse]
    putIOwords ["decoded", showT r]
    assertEqual res (show r)

res = "Just [FlickrResponse {photoset = Photoset \
\{photosetid = \"72157694654577774\", primary = \"9774229501\", \
\owner = \"101926234@N03\", ownername = \"mayer_tom\", photo = \
\[Photo {photoid = \"9771234001\", secret = \"82sd0454a2\", \
\server = \"7348\", farm = 7, title = \"TESTConf test\", \
\isprimary = \"0\", ispublic = 1, isfriend = 0, isfamily = 0}], \
\page = 1, per_page = 500, perpage = 500, pages = 1, total = \"34\", \
\name = \"TESTConf test\"}, stat = \"ok\"}]"



test_2 = assertEqual 6 6


