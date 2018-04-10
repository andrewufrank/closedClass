-----------------------------------------------------------------------------
--
-- Module      :   a test for HTF framework
-- insert {-@ HTF_TESTS @-} for each import
-----------------------------------------------------------------------------
{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Lib.Lit_tests     where      -- must have Main (main) or Main where


--import System.Exit

import           Test.Framework

test_1 = assertBool True
