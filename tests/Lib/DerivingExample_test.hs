-------------------------------------------------------
-- Module      :   a test for HTF framework
-- insert {-@ HTF_TESTS @-} for each import
-------------------------------------------------------
{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE OverloadedStrings     #-}
module Lib.DerivingExample_test -- (closedMain)
    where


import           Test.Framework
import Lib.DerivingExample
import Lib.DerivingExampleDerive
import Data.Text

test_int = assertEqual 0 (zero::Int)
test_a = assertEqual (Ax "" :: Ax) zero

test_b = assertEqual (B1 0) zero

