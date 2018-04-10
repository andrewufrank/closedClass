-----------------------------------------------------------------------------
--
-- Module      :   a test for HTF framework
-- insert {-@ HTF_TESTS @-} for each import
-----------------------------------------------------------------------------
{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE OverloadedStrings     #-}

module Lib.Lit_tests     where      -- must have Main (main) or Main where


--import System.Exit

import           Test.Framework
import Data.Text
import FirstSub

test_1 = assertBool True
s0 = "aa" :: Text
s1 = "bbb"::Text
test_app2 = assertEqual "aabbb" $  appendTwo s0 s1
t0 = mkOne s0 :: T1
t1 = mkOne s1 :: T1
t9 = T1 s0 :: T1
test_mk1 = assertEqual (show t1) (show (mkOne s1 :: T1))
