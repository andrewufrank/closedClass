-----------------------------------------------------------------------------
--
-- Module      :   a test for HTF framework
-- insert {-@ HTF_TESTS @-} for each import
-----------------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE OverloadedStrings     #-}

module Main     where      -- must have Main (main) or Main where


--import System.Exit

import           FirstSub
import           Uniform.Convenience.StartApp
                    hiding ((<>) , (</>), (<.>))
programName = "testSome" :: Text
progTitle = "testing xx " :: Text

main :: IO ()
main = startProg programName progTitle
            ( firstMain
            )

