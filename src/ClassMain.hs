-----------------------------------------------------------------------------
--
-- Module      :   main for tests
-----------------------------------------------------------------------------
--{-# LANGUAGE FlexibleContexts      #-}
--{-# LANGUAGE FlexibleInstances     #-}
--{-# LANGUAGE MultiParamTypeClasses #-}
--{-# LANGUAGE ScopedTypeVariables   #-}
--{-# LANGUAGE TypeFamilies          #-}
--{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE OverloadedStrings     #-}

--{-# LANGUAGE DeriveGeneric #-}

module Main     where      -- must have Main (main) or Main where


--import System.Exit

--import           ClosedClass
--import           Lib.BlogExample
--import  Lib.Tutorial1
import  Lib.ParseJsonCoreNLP

import Uniform.Convenience.StartApp

main :: IO ()
main =  do  -- with tests in other modules
        main2
        -- insert here modules in IO
--    tutorial1Main
--    blogMain
--    closedMain
--    blogMain

programName = "ClassMain - from SomeTest"
progTitle = "test Example"

main2 :: IO ()
main2 = startProg programName   progTitle
    (do
--        tutorial1Main
--        blogMain
        parseNLP
    )

