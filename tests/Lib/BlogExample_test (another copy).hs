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
module BlogExample_test  -- (openMain, htf_thisModuelsTests)
     where


import           Test.Framework
import           Uniform.Strings



d1 = Data1 "eines"
d2 = Data2 "zwei"
d3 = Data3 3

-- show produces the "xx"
test_1 = assertEqual 7 (lengthChar $ op1 d1)
test_2 = assertEqual 6 (lengthChar $ op1 d2)




