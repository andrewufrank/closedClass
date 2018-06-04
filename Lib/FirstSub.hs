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

module FirstSub
    where

import Uniform.Strings
import Test.Framework

--import Data.Text as T
--import Data.Text.IO as T
--
--import Control.Monad.Error  -- is monads-tf
import Distinction

firstMain :: IO ()
firstMain = return ()

--test_F0 = assertEqual  True True
