-----------------------------------------------------------------------------
--
-- Module      :   a test for HTF framework
-- insert {-@ HTF_TESTS @-} for each import
-----------------------------------------------------------------------------
{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Main     where      -- must have Main (main) or Main where


--import System.Exit

import           Test.Framework

import {-@ HTF_TESTS @-} Data.Utilities

import {-@ HTF_TESTS @-} NLP.Corpora.BrownTests
import {-@ HTF_TESTS @-} NLP.Corpora.ConllTests

main =  do  -- with tests in other modules
    putStrLn "HTF ExampleTest.hs:\n"
    p <- htfMain htf_importedTests
    putStrLn ("HTF end TreeTagSetsTesting.hs test:\n" ++ show p)
    return ()

