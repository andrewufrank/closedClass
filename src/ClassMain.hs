-----------------------------------------------------------------------------
--
-- Module      :   a test for HTF framework
-- insert {-@ HTF_TESTS @-} for each import
-----------------------------------------------------------------------------
--{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Main     where      -- must have Main (main) or Main where


--import System.Exit

--import           Lib.ClosedClass
import           Lib.PrettyPrintTest

main :: IO ()
main =  do  -- with tests in other modules
--    closedMain
    prttyMain

