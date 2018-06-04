-----------------------------------------------------------------------------
--
-- Module      :   a test for HTF framework
-- insert {-@ HTF_TESTS @-} for each import
-----------------------------------------------------------------------------
{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Main     where      -- must have Main (main) or Main where


--import System.Exit

import           Test.Framework
--import {-@ HTF_TESTS @-} FirstSub   -- pay attention to HTF_TESTS !
--import {-@ HTF_TESTS @-} Distinction
--import {-@ HTF_TESTS @-} Belnap
import {-@ HTF_TESTS @-} Taxon

main =  do  -- with tests in other modules
    putStrLn "HTF ExampleTest.hs:\n"
    p <- htfMain htf_importedTests
    putStrLn ("HTF end StringConversion.hs test:\n" ++ show p)
    return ()

