-----------------------------------------------------------------------------
--
-- Module      :  the distinction algebra
-- insert {-@ HTF_TESTS @-} for each import
-----------------------------------------------------------------------------
{-# OPTIONS_GHC -F -pgmF htfpp #-}

{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE DeriveAnyClass     #-}

module Distinction
    where

import Test.Framework
import Uniform.Strings
import Uniform.Error
--import GHC.Generic

import Belnap

class Distinctions d where
    sub, super :: d -> d -> Bool
--    order :: d -> d -> Ordering  -- use compare from Ord
    join, meet :: d -> d -> d


