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
t0 = "aa" :: Text
t1 = "bbb"::Text
test_appText = assertEqual "aabbb" $  appendTwo t0 t1
--t0 = mkOne s0 :: T1
--t1 = mkOne s1 :: T1
--t9 = T1 s0 :: T1
--test_mk1 = assertEqual (show t1) (show (mkOne s1 :: T1))

--s0 = 'a' -- "aa" :: String
--s1 = 'b' -- "bbb"::String
s0 = "aa" :: String
s1 = "bbb"::String
test_app2 = assertEqual ("aabbb"::String) $  (appendTwo s0 s1 :: String)
--t0 = mkOne s0 :: T1
--t1 = mkOne s1 :: T1
--t9 = T1 s0 :: T1
--test_mk1 = assertEqual (show t1) (show (mkOne s1 :: T1))
